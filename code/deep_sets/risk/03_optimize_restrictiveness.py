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
theory = sys.argv[3]
# num points = number of points per optimized set
num_points = int(sys.argv[4])

num_sets = 50
r = 100

# Open log files with num_points in the filename
odir = 'data_k{0}_nsim{1}_{2}/outfiles/'.format(k, nsims, theory)
# f1 = open(odir + f'03_optimize_garp_test_stdout_{num_points}.log', 'w')
# f2 = open(odir + f'03_optimize_garp_test_stderr_{num_points}.log', 'w')
# sys.stdout = f1
# sys.stderr = f2

# Load the trained model
model_path = 'data_k{0}_nsim{1}_{2}/best_mse_model.pth'.format(k, nsims, theory)
model = torch.load(model_path)
model.eval()  # Set the model to evaluation mode

# Define the objective function to minimize
def objective_function(points):
    # Reshape points to (num_points, 3)
    points = points.reshape(num_points, 3)
    # Convert points to a tensor and move to the appropriate device
    points_tensor = torch.from_numpy(points).float().to(next(model.parameters()).device)
    # Add a batch dimension and pass through the model
    with torch.no_grad():
        prediction = -1 * model(points_tensor.unsqueeze(0)).item()
    return prediction

# Bounds for the points (each coordinate should be in [0, 100])
bounds = [(0.01, 0.99), (1, 150), (1, 150)]

# Options for the optimizer
options = {
    'maxiter': 10000,  # Maximum number of iterations
    'ftol': 1e-9,     # Tolerance for termination by the change of the function value
    'gtol': 1e-9      # Tolerance for termination by the norm of the gradient
}

predictions = []
odir = 'data_k{0}_nsim{1}_{2}/optimized_points_{3}/'.format(k, nsims, theory, num_points)
# Make odir if doesn't exist
os.makedirs(odir, exist_ok=True)

# Do in loop
for i in range(200000):
    np.random.seed(i)
    
    # Initial guess for the points (num_points points in [0, 100]^2)
    initial_guess = np.random.uniform(0, r, size=(num_points, 2)).flatten()

    inital_guess_p = np.random.uniform(0, 1, size = (num_points, 1))
    # round to 2 decimal places
    inital_guess_p = np.round(inital_guess_p, 2)
    initial_guess_w = np.random.uniform(1, 150, size = (num_points, 1))
    # round to integer
    initial_guess_w = np.round(initial_guess_w)
    # sample num_points uniformly between 0 and initial_guess_w
    initial_guess_l = np.random.uniform(0, initial_guess_w, size = (num_points, 1))
    # round to integer
    initial_guess_l = np.round(initial_guess_l)
    initial_guess = np.concatenate((inital_guess_p, initial_guess_w, initial_guess_l)).flatten()


    # Define bounds
    bounds = (
        [(0, 1)] * num_points +       # Bounds for p_i: 0 ≤ p_i ≤ 1
        [(1, 150)] * num_points +     # Bounds for w_i: 1 ≤ w_i ≤ 150
        [(0, 150)] * num_points       # Bounds for l_i: 0 ≤ l_i ≤ 150 (temporary, constraint will handle l_i ≤ w_i)
    )

    # Define constraint for l_i ≤ w_i
    def constraint_l_w(x):
        """ Constraint function ensuring l_i ≤ w_i for all i. """
        w_values = x[num_points:2*num_points]  # Extract w values
        l_values = x[2*num_points:]  # Extract l values
        return w_values - l_values  # Must be ≥ 0 (i.e., w_i - l_i ≥ 0)

    # Set up constraint dictionary
    constraints = {'type': 'ineq', 'fun': constraint_l_w}

    # Run optimization
    result = minimize(
        objective_function,
        initial_guess,
        bounds=bounds,
        constraints=constraints,  # Enforce l_i ≤ w_i
        method='L-BFGS-B',  # Suitable for bounded optimization
        options=options
    )

    # Extract the optimized points
    optimized_points = result.x.reshape(num_points, 3, order = 'F')
    pdb.set_trace()

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
            'p': optimized_points[:, 0],
            'w': optimized_points[:, 1],
            'l': optimized_points[:, 2],
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
            'p': optimized_points[:, 0],
            'w': optimized_points[:, 1],
            'l': optimized_points[:, 2],
            'prediction': [result.fun] * num_points
        }
        # Create a DataFrame and write to CSV
        df = pd.DataFrame(data)

        df.to_csv(odir+ f'optimized_points_{max_index}.csv', index=False)

data = {'predictions': predictions}
df = pd.DataFrame(data)
df.to_csv(odir + 'predictions.csv', index=False)

# f1.close()
# f2.close()
