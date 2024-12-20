import yfinance as yf
import numpy as np
import pandas as pd

# Example ticker
ticker = "AAPL"

# Create a Ticker object
stock = yf.Ticker(ticker)

# Retrieve balance sheet and financials
balance_sheet = stock.balance_sheet
financials = stock.financials

# Metrics to calculate
balance_sheet_metrics = ['Total Debt', 'Total Assets', "Stockholders Equity"]
derived_metrics = ['Debt-to-Assets Ratio', 'ROA', 'ROE']

# Periods to compare
start_period = "2023-09-30"
end_period = "2024-09-30"

# Initialize a dictionary to store the results
results = {'Metric': [], f'{start_period} Value': [], f'{end_period} Value': [], 'Change (%)': []}

# Ensure balance_sheet and financials are available
if balance_sheet is not None and financials is not None:
    for metric in balance_sheet_metrics:
        try:
            # Ensure the metric exists in the balance_sheet DataFrame
            if metric in balance_sheet.index:
                start_value = balance_sheet.loc[metric, start_period]
                end_value = balance_sheet.loc[metric, end_period]

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
                print(f"Metric {metric} not found in balance sheet.")
        except Exception as e:
            print(f"Error processing {metric}: {e}")

    # Calculate derived metrics
    try:
        # Extract required values
        total_debt_start = balance_sheet.loc['Total Debt', start_period]
        total_assets_start = balance_sheet.loc['Total Assets', start_period]
        stockholders_equity_start = balance_sheet.loc["Stockholders Equity", start_period]
        net_income_start = financials.loc['Net Income', start_period] if 'Net Income' in financials.index else np.nan

        total_debt_end = balance_sheet.loc['Total Debt', end_period]
        total_assets_end = balance_sheet.loc['Total Assets', end_period]
        stockholders_equity_end = balance_sheet.loc["Stockholders Equity", end_period]
        net_income_end = financials.loc['Net Income', end_period] if 'Net Income' in financials.index else np.nan

        # Debt-to-Assets Ratio
        debt_to_assets_start = (total_debt_start / total_assets_start) * 100 if total_assets_start else np.nan
        debt_to_assets_end = (total_debt_end / total_assets_end) * 100 if total_assets_end else np.nan
        debt_to_assets_change = ((debt_to_assets_end - debt_to_assets_start) / debt_to_assets_start) * 100 if debt_to_assets_start else np.nan

        # ROA
        roa_start = (net_income_start / total_assets_start) * 100 if total_assets_start else np.nan
        roa_end = (net_income_end / total_assets_end) * 100 if total_assets_end else np.nan
        roa_change = ((roa_end - roa_start) / roa_start) * 100 if roa_start else np.nan

        # ROE
        roe_start = (net_income_start / stockholders_equity_start) * 100 if stockholders_equity_start else np.nan
        roe_end = (net_income_end / stockholders_equity_end) * 100 if stockholders_equity_end else np.nan
        roe_change = ((roe_end - roe_start) / roe_start) * 100 if roe_start else np.nan

        # Append derived metrics
        for metric, start_value, end_value, change in zip(
            derived_metrics,
            [debt_to_assets_start, roa_start, roe_start],
            [debt_to_assets_end, roa_end, roe_end],
            [debt_to_assets_change, roa_change, roe_change],
        ):
            results['Metric'].append(metric)
            results[f'{start_period} Value'].append(start_value)
            results[f'{end_period} Value'].append(end_value)
            results['Change (%)'].append(change)
    except Exception as e:
        print(f"Error calculating derived metrics: {e}")
else:
    print("Balance sheet or financial data not available for this ticker.")

# Convert results to a DataFrame for display
results_df = pd.DataFrame(results)

# Display the DataFrame
print(results_df)

# Optionally save the DataFrame to a CSV
results_df.to_csv(f"{ticker}_balance_sheet_changes.csv", index=False)
