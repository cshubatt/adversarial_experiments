import pdb
import numpy as np
from tqdm import tqdm, trange
import pandas as pd

class DataIterator(object):
    def __init__(self, fname, batch_size, shuffle=False):
        self.fname = fname
        self.batch_size = batch_size
        self.shuffle = shuffle
        self.load_data(fname)
        
    def load_data(self, fname):
        # Read the CSV file
        data = pd.read_csv(fname)
        
        # Extract the number of elements k
        k = len([col for col in data.columns if col.startswith('p')])
        
        # Extract p, w, l columns
        p_columns = [f'p_{i+1}' for i in range(k)]
        w_columns = [f'w_{i+1}' for i in range(k)]
        l_columns = [f'l_{i+1}' for i in range(k)]
        
        # Create self.X with dimensions (# rows, k, d)
        d = 3
        self.X = np.zeros((data.shape[0], k, d))
        self.X[:, :, 0] = data[p_columns].values
        self.X[:, :, 1] = data[w_columns].values
        self.X[:, :, 2] = data[l_columns].values
        self.d = d

        # Set self.y to the ccei column
        self.y = data["mean_restrictiveness"].values
        
        assert len(self.y) >= self.batch_size, \
            'Batch size larger than number of training examples'

            
    def __len__(self):
        return len(self.y)//self.batch_size

    def get_iterator(self, loss=0.0):
        if self.shuffle:
            rng_state = np.random.get_state()
            np.random.shuffle(self.X)
            np.random.set_state(rng_state)
            np.random.shuffle(self.y)
            np.random.set_state(rng_state)
        return tqdm(self.next_batch(),
                    desc='Train loss: {:.4f}'.format(loss),
                    total=len(self), mininterval=1.0, ncols=80)
                    
    def next_batch(self):
        start = 0
        end = self.batch_size
        while end <= self.X.shape[0]:
            yield self.X[start:end], self.y[start:end]
            start = end
            end += self.batch_size
