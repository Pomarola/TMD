import yfinance as yf
import numpy as np
import pandas as pd

# Example ticker
ticker = "AAPL"

# Create a Ticker object
stock = yf.Ticker(ticker)

# Retrieve cashflow
cashflow = stock.cashflow

# Metrics to calculate
cashflow_metrics = [
    'Free Cash Flow', 'Repurchase Of Capital Stock', 'Repayment Of Debt',
    'Issuance Of Debt', 'Issuance Of Capital Stock', 'Capital Expenditure'
]

# Periods to compare
start_period = "2023-09-30"
end_period = "2024-09-30"

# Initialize a dictionary to store the results
results = {'Metric': [], f'{start_period} Value': [], f'{end_period} Value': [], 'Change (%)': []}

# Ensure cashflow is available
if cashflow is not None:
    for metric in cashflow_metrics:
        try:
            # Ensure the metric exists in the cashflow DataFrame
            if metric in cashflow.index:
                start_value = cashflow.loc[metric, start_period] if start_period in cashflow.columns else np.nan
                end_value = cashflow.loc[metric, end_period] if end_period in cashflow.columns else np.nan

                # Calculate percentage change
                if start_value != 0 and not np.isnan(start_value):  # Avoid division by zero
                    change_pct = ((end_value - start_value) / start_value) * 100
                else:
                    change_pct = np.nan  # Handle zero or missing start value gracefully

                # Append to results
                results['Metric'].append(metric)
                results[f'{start_period} Value'].append(start_value)
                results[f'{end_period} Value'].append(end_value)
                results['Change (%)'].append(change_pct)
            else:
                print(f"Metric {metric} not found in cashflow.")
        except Exception as e:
            print(f"Error processing {metric}: {e}")
else:
    print(f"Cashflow data not available for {ticker}.")

# Convert results to a DataFrame for display
results_df = pd.DataFrame(results)

# Display the DataFrame
print(results_df)

# Optionally save to CSV
results_df.to_csv(f"{ticker}_cashflow_changes.csv", index=False)
