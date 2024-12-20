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

# Retrieve data
balance_sheet = stock.balance_sheet
financials = stock.financials
cashflow = stock.cashflow
historical_data = stock.history(period="1d", start="2023-09-29", end="2024-09-30")

# Initialize a dictionary to store combined results
results = {
    'Metric': [],
    'Change (%)': []
}

# ----------------------------
# 1. Enterprise Value and Related Metrics
# ----------------------------
try:
    # Retrieve Ordinary Shares Number
    shares_start = balance_sheet.loc['Ordinary Shares Number', start_period] if 'Ordinary Shares Number' in balance_sheet.index and start_period in balance_sheet.columns else np.nan
    shares_end = balance_sheet.loc['Ordinary Shares Number', end_period] if 'Ordinary Shares Number' in balance_sheet.index and end_period in balance_sheet.columns else np.nan

    close_price_start = historical_data.loc["2023-09-29", 'Close'] if "2023-09-29" else np.nan
    close_price_end = historical_data.loc["2024-09-27", 'Close'] if "2024-09-27" else np.nan

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

    # Append Enterprise Value Results
    ev_metrics = ['Market Capitalization', 'Total Debt', 'Cash and Cash Equivalents', 'Enterprise Value']
    ev_start_values = [market_cap_start, total_debt_start, cash_start, ev_start]
    ev_end_values = [market_cap_end, total_debt_end, cash_end, ev_end]
    ev_changes = [market_cap_change_pct, total_debt_change_pct, cash_change_pct, ev_change_pct]

    for metric, start_value, end_value, change in zip(ev_metrics, ev_start_values, ev_end_values, ev_changes):
        results['Metric'].append(metric)
        results['Change (%)'].append(change)

except Exception as e:
    print(f"Error calculating Enterprise Value: {e}")

# ----------------------------
# 2. Financial Changes Calculation
# ----------------------------
financial_metrics = ['EBITDA', 'Total Revenue', 'Gross Profit', 'Operating Revenue', 'Operating Income', 'Net Income']
if financials is not None:
    for metric in financial_metrics:
        try:
            if metric in financials.index:
                start_value = financials.loc[metric, start_period]
                end_value = financials.loc[metric, end_period]

                change_pct = ((end_value - start_value) / start_value) * 100 if start_value != 0 else np.nan

                # Append results
                results['Metric'].append(metric)
                results['Change (%)'].append(change_pct)
        except Exception as e:
            print(f"Error processing financial metric {metric}: {e}")

# ----------------------------
# 3. Cash Flow Changes Calculation
# ----------------------------
cashflow_metrics = ['Free Cash Flow', 'Repurchase Of Capital Stock', 'Repayment Of Debt', 'Issuance Of Debt', 'Issuance Of Capital Stock', 'Capital Expenditure']
if cashflow is not None:
    for metric in cashflow_metrics:
        try:
            if metric in cashflow.index:
                start_value = cashflow.loc[metric, start_period] if start_period in cashflow.columns else np.nan
                end_value = cashflow.loc[metric, end_period] if end_period in cashflow.columns else np.nan

                change_pct = ((end_value - start_value) / start_value) * 100 if start_value != 0 else np.nan

                # Append results
                results['Metric'].append(metric)
                results['Change (%)'].append(change_pct)
        except Exception as e:
            print(f"Error processing cashflow metric {metric}: {e}")

# ----------------------------
# 4. Balance Sheet Metrics and Derived Metrics
# ----------------------------
balance_sheet_metrics = ['Total Assets', "Stockholders Equity"]

if balance_sheet is not None:
    for metric in balance_sheet_metrics:
        try:
            if metric in balance_sheet.index:
                start_value = balance_sheet.loc[metric, start_period]
                end_value = balance_sheet.loc[metric, end_period]

                change_pct = ((end_value - start_value) / start_value) * 100 if start_value != 0 else np.nan

                # Append results
                results['Metric'].append(metric)
                results['Change (%)'].append(change_pct)
        except Exception as e:
            print(f"Error processing balance sheet metric {metric}: {e}")

# ----------------------------
# 5. Add Stock Price Change Row
# ----------------------------
try:
    price_start = stock.history(start="2023-10-30", end="2023-10-31")['Close'].iloc[0]
    price_end = stock.history(start="2024-10-30", end="2024-10-31")['Close'].iloc[0]

    price_change_pct = ((price_end - price_start) / price_start) * 100 if price_start != 0 else np.nan

    results['Metric'].append("Stock Price Change")
    results['Change (%)'].append(price_change_pct)

except Exception as e:
    print(f"Error calculating stock price change: {e}")

# ----------------------------
# Output Results
# ----------------------------
results_df = pd.DataFrame(results)

# Display the DataFrame
print(results_df)

# Optionally save to CSV
results_df.to_csv(f"{ticker}_combined_metrics.csv", index=False)
