import os
import pdb
import sys
import numpy as np
from tqdm import tqdm, trange

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import torch
import torch.nn as nn
import torch.optim as optim
from torch.autograd import Variable

import tensorflow as tf

from loader import DataIterator
from model import DeepSet

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

def log_scalar(writer, tag, value, step):
    with writer.as_default():
        tf.summary.scalar(tag, value, step=step)
    writer.flush()  # Ensure the data is written to disk
    
class Trainer(object):

    def __init__(self, in_dims, num_epochs=1000, out_dir='out/', log_dir='logs/'):        
        self.model = DeepSet(in_dims).to(device)
        
        self.l1 = nn.L1Loss()
        self.l2 = nn.MSELoss()
        
        self.optim = optim.Adam(self.model.parameters(), lr=0.001)
        self.scheduler = optim.lr_scheduler.ReduceLROnPlateau(self.optim, factor=0.5, patience=50, verbose=True)
        
        self.num_epochs = num_epochs
        self.log_dir = log_dir
        self.out_dir = out_dir
            
    def fit(self, train, valid=None):
        train_loss = 0.0
        best_mae = 1.0e3
        best_mse = 1.0e6
        loss_val = 0.0

        train_writer = tf.summary.create_file_writer(self.log_dir)

        for j in trange(self.num_epochs, desc="Epochs: ", ncols=80):
            train_iterator = train.get_iterator(train_loss)
            for X, y in train_iterator:
                # pdb.set_trace()
                self.optim.zero_grad()
                X_tensor = torch.from_numpy(X).float().to(device)  # Ensure the input is float32
                y_tensor = torch.from_numpy(y).float().to(device)  # Ensure the target is float32
                y_pred = self.model(Variable(X_tensor)).view(-1)
                loss = self.l1(y_pred, y_tensor)  # Use self.l2 or self.l1 as needed
                loss_val = loss.data.cpu().numpy().item()
                train_loss = 0.9 * train_loss + 0.1 * loss_val
                loss.backward()
                self.optim.step()
                train_iterator.set_description(
                    'Train loss: {0:.4f}'.format(train_loss))

            test_mae, test_mse = self.evaluate(valid)
            # pdb.set_trace()
            log_scalar(train_writer, 'train_loss', train_loss, j + 1)
            log_scalar(train_writer, 'test_mae', test_mae, j + 1)
            log_scalar(train_writer, 'test_mse', test_mse, j + 1)

            if test_mae < best_mae:
                best_mae = test_mae
                torch.save(self.model, self.out_dir + 'finetune_best_mae_model.pth')
            if test_mse < best_mse:
                best_mse = test_mse
                torch.save(self.model, self.out_dir + 'finetune_best_mse_model.pth')

        return best_mae, best_mse        
        
    def evaluate(self, test):
        counts = 0
        sum_mae = 0.0
        sum_mse = 0.0
        test_iterator = test.get_iterator()
        
        for X, y in test_iterator:
            counts += 1
            # Move data to the selected device (CPU or GPU)
            X_tensor = torch.from_numpy(X).float().to(device)
            y_tensor = torch.from_numpy(y).float().to(device)
            
            y_pred = self.model(X_tensor).view(-1)
            sum_mae += self.l1(y_pred, y_tensor).data.cpu().numpy()
            sum_mse += self.l2(y_pred, y_tensor).data.cpu().numpy()
        
        return (sum_mae / counts).item(), (sum_mse / counts).item()

    def predict(self, test):
        y_preds = []
        for X, y in test.next_batch():
            X_tensor = torch.from_numpy(X).float().to(device)
            y_pred = self.model(X_tensor)
            y_preds.append(y_pred.data.cpu().numpy())
        return np.concatenate(y_preds).reshape(-1)
    
    def save_predictions(self, test, file_name):
        y_preds = []
        for X, y in test.next_batch():
            X_tensor = torch.from_numpy(X).float().to(device)
            y_pred = self.model(X_tensor)
            y_preds.append(y_pred.data.cpu().numpy())
        np.savetxt(file_name, np.concatenate(y_preds), delimiter=",", fmt='%0.6f')
        return np.concatenate(y_preds).reshape(-1)    
        
if __name__ == '__main__':
    
    k = int(sys.argv[1])
    nsims = int(sys.argv[2])
    # num points = number of points per optimized set
    num_points = int(sys.argv[3])

    load_dir = 'data_k{0}_nsim{1}/'.format(k, nsims)
    ddir = 'data_k{0}_nsim{1}/optimized_points_{2}/'.format(k, nsims, num_points)
    odir = load_dir + 'outfiles/'
    
    f1 = open(odir + '05_finetune_stdout_{0}.txt'.format(num_points), 'w')
    f2 = open(odir + '05_finetune_stderr_{0}.txt'.format(num_points), 'w')
    sys.stdout = f1
    sys.stderr = f2
    print('Running estimation for k = {0}, nsims = {1}'.format(k, nsims))
    
    # Load dataset
    train = DataIterator(ddir+'finetune_train.csv', 128, shuffle=True)
    valid = DataIterator(ddir+'finetune_valid.csv', 128)
    assert train.d == valid.d and train.d, \
            'Dimensions of train, valid do not match!'

    
    nb_epoch = 500 # np.max([1024*1024/train.L,100])
    print(train.d)
    t = Trainer(train.d, nb_epoch, ddir, ddir+'logs/')
    a, b = t.fit(train, valid)
    t.model = torch.load(load_dir + 'best_mse_model.pth')
    a, b = t.evaluate(valid)
    print('Valid set evaluation: ')
    print('\t MAE: {0:0.6g} \t MSE: {1:0.6g}'.format(a, b))

    # Save predictions for train, validation, and test sets
    preds_train = t.save_predictions(train, ddir + 'train_predictions.csv')
    preds_valid = t.save_predictions(valid, ddir + 'valid_predictions.csv')

    print("TRAIN COR:")
    print(np.corrcoef(preds_train, train.y[0:len(preds_train)]))
    print("VALID COR:")
    print(np.corrcoef(preds_valid, valid.y[0:len(preds_valid)]))
    f1.close()
    f2.close()

