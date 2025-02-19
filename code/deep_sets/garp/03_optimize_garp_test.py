import torch
import torch.nn as nn
import numpy as np
from scipy.optimize import minimize
import pdb
import pandas as pd
import sys
import os

k = int(sys.argv[1])
nsims = int(sys.argv[2])
# num points = number of points per optimized set
num_points = int(sys.argv[3])
y_var = sys.argv[4]

num_sets = 50
r = 100

# Open log files with num_points in the filename
odir = 'data_k{0}_nsim{1}_{2}/outfiles/'.format(k, nsims, y_var)
f1 = open(odir + f'03_optimize_garp_test_stdout_{num_points}.log', 'w')
f2 = open(odir + f'03_optimize_garp_test_stderr_{num_points}.log', 'w')
sys.stdout = f1
sys.stderr = f2

# Load the trained model
model_path = 'data_k{0}_nsim{1}_{2}/best_mse_model.pth'.format(k, nsims, y_var)
model = torch.load(model_path)
model.eval()  # Set the model to evaluation mode

# Define the objective function to minimize
def objective_function(points):
    # Reshape points to (num_points, 2)
    points = points.reshape(num_points, 2)
    # Convert points to a tensor and move to the appropriate device
    points_tensor = torch.from_numpy(points).float().to(next(model.parameters()).device)
    # Add a batch dimension and pass through the model
    with torch.no_grad():
        prediction = model(points_tensor.unsqueeze(0)).item()
    return prediction

# Bounds for the points (each coordinate should be in [0, 100])
bounds = [(1, 100) for _ in range(num_points * 2)]

# Options for the optimizer
options = {
    'maxiter': 10000,  # Maximum number of iterations
    'ftol': 1e-9,     # Tolerance for termination by the change of the function value
    'gtol': 1e-9      # Tolerance for termination by the norm of the gradient
}

predictions = []
odir = 'data_k{0}_nsim{1}_{2}/optimized_points_{3}/'.format(k, nsims, y_var, num_points)
# Make odir if doesn't exist
os.makedirs(odir, exist_ok=True)

# Do in loop
for i in range(20000):
    np.random.seed(i)
    
    # Initial guess for the points (num_points points in [0, 100]^2)
    initial_guess = np.random.uniform(0, r, size=(num_points, 2)).flatten()
    # # subtract r/2, divide by sqrt(r^2/12)
    # initial_guess_norm = (initial_guess - r/2) / np.sqrt(r**2/12)
    
    # lb = (0 - r/2) / np.sqrt(r**2/12)
    # ub = (100 - r/2) / np.sqrt(r**2/12)
    # bounds_norm = [(lb, ub) for _ in range(num_points * 2)]

    # # Perform the optimization
    # result = minimize(objective_function, initial_guess_norm, bounds=bounds_norm, method='L-BFGS-B', options=options)
    # pdb.set_trace()

    # Initial guess for the points (num_points points in [0, 100]^2)
    initial_guess = np.random.uniform(1, 100, size=(num_points, 2)).flatten()

    # Perform the optimization
    result = minimize(
        objective_function, initial_guess, bounds=bounds, method='L-BFGS-B', options=options
        )

    # Extract the optimized points
    optimized_points = result.x.reshape(num_points, 2)

    # Keep only num_sets best points
    if len(predictions) < num_sets:
        predictions = np.append(predictions, result.fun)

        # Print the optimized points and the minimized value
        print("Optimized Points:")
        print(optimized_points)
        print("Minimized Value:")
        print(result.fun)

        # Prepare data for CSV
        data = {
            'x': optimized_points[:, 0],
            'y': optimized_points[:, 1],
            'prediction': [result.fun] * num_points
        }
        # Create a DataFrame and write to CSV
        df = pd.DataFrame(data)

        df.to_csv(odir+ f'optimized_points_{i}.csv', index=False)
    elif result.fun > np.max(predictions):
        continue
    else:
        # Replace max value with new value
        max_index = np.argmax(predictions)
        predictions[max_index] = result.fun

        # Prepare data for CSV
        data = {
            'x': optimized_points[:, 0],
            'y': optimized_points[:, 1],
            'prediction': [result.fun] * num_points
        }
        # Create a DataFrame and write to CSV
        df = pd.DataFrame(data)

        df.to_csv(odir+ f'optimized_points_{max_index}.csv', index=False)

data = {'predictions': predictions}
df = pd.DataFrame(data)
df.to_csv(odir + 'predictions.csv', index=False)

f1.close()
f2.close()
