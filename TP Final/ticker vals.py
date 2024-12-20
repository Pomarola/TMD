import yfinance as yf
import numpy as np

# Example ticker
ticker = "AAPL"

# Create a Ticker object
stock = yf.Ticker(ticker)

# Retrieve financials
financials = stock.financials

# Metrics to calculate the percentage change
metrics = ['EBITDA', 'Basic EPS', 'Total Revenue', 'Gross Profit', 'Operating Revenue', 'Operating Income']

# Periods to compare
start_period = "2023-09-30"
end_period = "2024-09-30"

# Initialize a dictionary to store the results
results = {'Metric': [], f'{start_period} Value': [], f'{end_period} Value': [], 'Change (%)': []}

if financials is not None:
    for metric in metrics:
        try:
            # Ensure the metric exists in the financials DataFrame
            if metric in financials.index:
                start_value = financials.loc[metric, start_period]
                end_value = financials.loc[metric, end_period]

                # Calculate percentage change
                if start_value != 0:  # Avoid division by zero
                    change_pct = ((end_value - start_value) / start_value) * 100
                else:
                    change_pct = np.nan  # Handle zero start value gracefully

                # Append to results
                results['Metric'].append(metric)
                results[f'{start_period} Value'].append(start_value)
                results[f'{end_period} Value'].append(end_value)
                results['Change (%)'].append(change_pct)
            else:
                print(f"Metric {metric} not found in financials.")
        except Exception as e:
            print(f"Error processing {metric}: {e}")
else:
    print("No financial data available for this ticker.")

# Convert results to a DataFrame for display
import pandas as pd
results_df = pd.DataFrame(results)

# Display the DataFrame
print(results_df)

# Optionally save the DataFrame to a CSV
results_df.to_csv(f"{ticker}_financial_changes.csv", index=False)
