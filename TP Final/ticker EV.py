import yfinance as yf
import numpy as np
import pandas as pd

# Example ticker
ticker = "AAPL"

# Create a Ticker object
stock = yf.Ticker(ticker)

# Periods for analysis
start_period = "2023-09-30"
end_period = "2024-09-30"

# Retrieve balance sheet and historical data
balance_sheet = stock.balance_sheet
historical_data = stock.history(period="1d", start="2023-09-29", end="2024-09-30")

# Initialize a dictionary to store the results
results = {'Metric': ['Market Capitalization', 'Total Debt', 'Cash and Cash Equivalents', 'Enterprise Value'],
           f'{start_period} Value': [np.nan] * 4,
           f'{end_period} Value': [np.nan] * 4,
           'Change (%)': [np.nan] * 4}

try:
    # Retrieve Ordinary Shares Number
    shares_start = balance_sheet.loc['Ordinary Shares Number', start_period] if 'Ordinary Shares Number' in balance_sheet.index and start_period in balance_sheet.columns else np.nan
    shares_end = balance_sheet.loc['Ordinary Shares Number', end_period] if 'Ordinary Shares Number' in balance_sheet.index and end_period in balance_sheet.columns else np.nan
    
    # Retrieve closing prices for specific dates
    close_price_start = historical_data.loc['2023-09-29', 'Close'] if '2023-09-29' in historical_data.index else np.nan
    close_price_end = historical_data.loc['2024-09-27', 'Close'] if '2024-09-27' in historical_data.index else np.nan
    
    # Calculate Market Capitalization
    market_cap_start = close_price_start * shares_start if not np.isnan(close_price_start) and not np.isnan(shares_start) else np.nan
    market_cap_end = close_price_end * shares_end if not np.isnan(close_price_end) and not np.isnan(shares_end) else np.nan
    
    # Retrieve Total Debt and Cash and Cash Equivalents
    total_debt_start = balance_sheet.loc['Total Debt', start_period] if 'Total Debt' in balance_sheet.index and start_period in balance_sheet.columns else np.nan
    total_debt_end = balance_sheet.loc['Total Debt', end_period] if 'Total Debt' in balance_sheet.index and end_period in balance_sheet.columns else np.nan
    cash_start = balance_sheet.loc['Cash And Cash Equivalents', start_period] if 'Cash And Cash Equivalents' in balance_sheet.index and start_period in balance_sheet.columns else np.nan
    cash_end = balance_sheet.loc['Cash And Cash Equivalents', end_period] if 'Cash And Cash Equivalents' in balance_sheet.index and end_period in balance_sheet.columns else np.nan

    # Calculate Enterprise Value
    ev_start = market_cap_start + total_debt_start - cash_start if not np.isnan(market_cap_start) and not np.isnan(total_debt_start) and not np.isnan(cash_start) else np.nan
    ev_end = market_cap_end + total_debt_end - cash_end if not np.isnan(market_cap_end) and not np.isnan(total_debt_end) and not np.isnan(cash_end) else np.nan

    # Calculate percentage changes
    ev_change_pct = ((ev_end - ev_start) / ev_start) * 100 if ev_start != 0 and not np.isnan(ev_start) else np.nan
    market_cap_change_pct = ((market_cap_end - market_cap_start) / market_cap_start) * 100 if market_cap_start != 0 and not np.isnan(market_cap_start) else np.nan
    total_debt_change_pct = ((total_debt_end - total_debt_start) / total_debt_start) * 100 if total_debt_start != 0 and not np.isnan(total_debt_start) else np.nan
    cash_change_pct = ((cash_end - cash_start) / cash_start) * 100 if cash_start != 0 and not np.isnan(cash_start) else np.nan

    # Store results
    results[f'{start_period} Value'] = [market_cap_start, total_debt_start, cash_start, ev_start]
    results[f'{end_period} Value'] = [market_cap_end, total_debt_end, cash_end, ev_end]
    results['Change (%)'] = [market_cap_change_pct, total_debt_change_pct, cash_change_pct, ev_change_pct]

except Exception as e:
    print(f"Error calculating Enterprise Value: {e}")

# Convert results to DataFrame
results_df = pd.DataFrame(results)

# Display results
print(results_df)

# Optionally save to CSV
results_df.to_csv(f"{ticker}_enterprise_value_changes.csv", index=False)
