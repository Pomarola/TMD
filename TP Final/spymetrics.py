import yfinance as yf
import numpy as np
import pandas as pd

# Scrape S&P 500 tickers from Wikipedia
wiki_url = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
sp500_table = pd.read_html(wiki_url)
sp500_tickers = sp500_table[0]['Symbol'].tolist()  # Adjust table index if necessary

print(sp500_tickers)

# Periods for analysis
start_period = "2022-09-30"
end_period = "2023-09-30"

# Initialize a DataFrame to store combined results
results = pd.DataFrame()

# Helper function to find valid closest date
def find_valid_date(df, target_date):
    if df is None or df.empty:
        return None
    try:
        available_dates = pd.to_datetime(df.columns)
        target_date = pd.to_datetime(target_date)
        closest_date = available_dates[available_dates <= target_date].max()
        return closest_date.strftime("%Y-%m-%d") if pd.notnull(closest_date) else None
    except Exception as e:
        return None

# ----------------------------
# Function to Analyze a Single Ticker
# ----------------------------
def analyze_ticker(ticker):
    stock = yf.Ticker(ticker)

    # Retrieve data
    balance_sheet = stock.balance_sheet
    financials = stock.financials
    cashflow = stock.cashflow
    historical_data = stock.history(period="1d", start="2022-09-29", end="2023-09-30")

    # Find closest valid dates
    start_date_bs = find_valid_date(balance_sheet, start_period)
    end_date_bs = find_valid_date(balance_sheet, end_period)

    start_date_fs = find_valid_date(financials, start_period)
    end_date_fs = find_valid_date(financials, end_period)

    start_date_cf = find_valid_date(cashflow, start_period)
    end_date_cf = find_valid_date(cashflow, end_period)

    # Initialize a dictionary to store results for the ticker
    ticker_results = {"Ticker": ticker}

    # ----------------------------
    # Enterprise Value and Related Metrics
    # ----------------------------
    try:
        shares_start = balance_sheet.loc['Ordinary Shares Number', start_date_bs] if start_date_bs else np.nan
        shares_end = balance_sheet.loc['Ordinary Shares Number', end_date_bs] if end_date_bs else np.nan

        close_price_start = historical_data.loc["2022-09-29", 'Close'] if "2022-09-29" in historical_data.index else np.nan
        close_price_end = historical_data.loc["2023-09-27", 'Close'] if "2023-09-27" in historical_data.index else np.nan

        market_cap_start = close_price_start * shares_start if not np.isnan(close_price_start) and not np.isnan(shares_start) else np.nan
        market_cap_end = close_price_end * shares_end if not np.isnan(close_price_end) and not np.isnan(shares_end) else np.nan

        total_debt_start = balance_sheet.loc['Total Debt', start_date_bs] if start_date_bs else np.nan
        total_debt_end = balance_sheet.loc['Total Debt', end_date_bs] if end_date_bs else np.nan
        cash_start = balance_sheet.loc['Cash And Cash Equivalents', start_date_bs] if start_date_bs else np.nan
        cash_end = balance_sheet.loc['Cash And Cash Equivalents', end_date_bs] if end_date_bs else np.nan

        ev_start = market_cap_start + total_debt_start - cash_start if not np.isnan(market_cap_start) else np.nan
        ev_end = market_cap_end + total_debt_end - cash_end if not np.isnan(market_cap_end) else np.nan

        ticker_results['Market Capitalization'] = ((market_cap_end - market_cap_start) / market_cap_start) * 100 if market_cap_start else np.nan
        ticker_results['Total Debt'] = ((total_debt_end - total_debt_start) / total_debt_start) * 100 if total_debt_start else np.nan
        ticker_results['Cash and Cash Equivalents'] = ((cash_end - cash_start) / cash_start) * 100 if cash_start else np.nan
        ticker_results['Enterprise Value'] = ((ev_end - ev_start) / ev_start) * 100 if ev_start else np.nan
    except Exception as e:
        print(f"Error calculating Enterprise Value for {ticker}: {e}")

    # ----------------------------
    # Financial Changes Calculation
    # ----------------------------
    financial_metrics = ['EBITDA', 'Total Revenue', 'Gross Profit', 'Operating Revenue', 'Operating Income', 'Net Income']
    if financials is not None:
        for metric in financial_metrics:
            try:
                start_value = financials.loc[metric, start_date_fs] if start_date_fs and metric in financials.index else np.nan
                end_value = financials.loc[metric, end_date_fs] if end_date_fs and metric in financials.index else np.nan

                ticker_results[metric] = ((end_value - start_value) / start_value) * 100 if start_value else np.nan
            except Exception as e:
                print(f"Error processing financial metric {metric} for {ticker}: {e}")

    # ----------------------------
    # Cash Flow Changes Calculation
    # ----------------------------
    cashflow_metrics = ['Free Cash Flow', 'Repurchase Of Capital Stock', 'Repayment Of Debt', 'Issuance Of Debt', 'Issuance Of Capital Stock', 'Capital Expenditure']
    if cashflow is not None:
        for metric in cashflow_metrics:
            try:
                start_value = cashflow.loc[metric, start_date_cf] if start_date_cf and metric in cashflow.index else np.nan
                end_value = cashflow.loc[metric, end_date_cf] if end_date_cf and metric in cashflow.index else np.nan

                ticker_results[metric] = ((end_value - start_value) / start_value) * 100 if start_value else np.nan
            except Exception as e:
                print(f"Error processing cashflow metric {metric} for {ticker}: {e}")

    # ----------------------------
    # Add Stock Price Change Row
    # ----------------------------
    try:
        # Fetch historical data for the full date range
        price_data = yf.download(ticker, start="2022-10-29", end="2023-11-02")

        # Ensure 'Adj Close' exists
        if "Adj Close" in price_data.columns:
            # Extract the adjusted close prices for the specific dates
            price_start = price_data.loc["2022-10-30":"2022-10-31", "Adj Close"].iloc[0]
            price_end = price_data.loc["2023-10-30":"2023-10-31", "Adj Close"].iloc[0]

            # Calculate stock price change
            ticker_results['Stock Price Change'] = ((price_end - price_start) / price_start) * 100
        else:
            print(f"Skipping {ticker}: 'Adj Close' not available")
            ticker_results['Stock Price Change'] = np.nan
    except Exception as e:
        print(f"Error calculating stock price change for {ticker}: {e}")
        ticker_results['Stock Price Change'] = np.nan

    return ticker_results

# ----------------------------
# Loop Through Tickers and Combine Results
# ----------------------------
all_results = []

for ticker in sp500_tickers:
    try:
        ticker_data = analyze_ticker(ticker)
        all_results.append(ticker_data)
    except Exception as e:
        print(f"Error analyzing {ticker}: {e}")

# Convert results to a DataFrame
results = pd.DataFrame(all_results)

# ----------------------------
# Output Results
# ----------------------------
print(results)

# Optionally save to CSV
results.to_csv("combined_metrics.csv", index=False)
