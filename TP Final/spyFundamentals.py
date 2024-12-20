import pandas as pd
import yfinance as yf
import numpy as np

# Define start and end dates for the analysis
start_date = "2023-06-30"
end_date = "2024-06-30"

# Scrape NASDAQ-100 tickers from Wikipedia
wiki_url = "https://en.wikipedia.org/wiki/NASDAQ-100"
nasdaq100_table = pd.read_html(wiki_url)
nasdaq100_tickers = nasdaq100_table[4]['Symbol'].tolist()  # Adjust table index if necessary

# Initialize a list to store the data
companies = []

# Iterate over each ticker
for ticker in nasdaq100_tickers:
    try:
        stock = yf.Ticker(ticker)
        
        # Retrieve historical price data for calculating annual return
        hist = stock.history(start=start_date, end=end_date)
        if not hist.empty:
            initial_price = hist['Close'].iloc[0]
            final_price = hist['Close'].iloc[-1]
            annual_return = ((final_price - initial_price) / initial_price) * 100
        else:
            annual_return = np.nan
            
        
        # Retrieve financial data
        info = stock.info
        financials = stock.financials if 'financials' in dir(stock) else None
        balance_sheet = stock.balance_sheet if 'balance_sheet' in dir(stock) else None
        cashflow = stock.cashflow if 'cashflow' in dir(stock) else None

        # Calculate metrics
        total_revenue = info.get('totalRevenue', np.nan)
        gross_profit = info.get('grossProfits', np.nan)
        operating_income = info.get('operatingIncome', np.nan)
        eps = info.get('trailingEps', np.nan)
        ebitda = info.get('ebitda', np.nan)
        total_debt = balance_sheet.loc['Total Debt', :].iloc[0] if balance_sheet is not None else np.nan
        total_assets = balance_sheet.loc['Total Assets', :].iloc[0] if balance_sheet is not None else np.nan
        debt_to_assets = (total_debt / total_assets) * 100 if total_debt and total_assets else np.nan
        roe = info.get('returnOnEquity', np.nan)
        roa = info.get('returnOnAssets', np.nan)
        free_cash_flow = cashflow.loc['Free Cash Flow', :].iloc[0] if cashflow is not None else np.nan
        pe_ratio = info.get('trailingPE', np.nan)
        beta = info.get('beta', np.nan)

        # Append to the list
        companies.append({
            'Ticker': ticker,
            'Total Revenue % Annual': total_revenue,
            'Gross Profit % Annual': gross_profit,
            'Operating Income % Annual': operating_income,
            'EPS % Annual': eps,
            'EBITDA % Annual': ebitda,
            'Debt-to-Assets Ratio % Annual': debt_to_assets,
            'ROE % Annual': roe,
            'ROA % Annual': roa,
            'FCF % Annual': free_cash_flow,
            'P/E Ratio in start_date': pe_ratio,
            'Beta in start_date': beta,
            'Annual Return %': annual_return
        })
    
    except Exception as e:
        print(f"Error processing {ticker}: {e}")
        # Append with NA for missing data
        companies.append({
            'Ticker': ticker,
            'Total Revenue % Annual': 'NA',
            'Gross Profit % Annual': 'NA',
            'Operating Income % Annual': 'NA',
            'EPS % Annual': 'NA',
            'EBITDA % Annual': 'NA',
            'Debt-to-Assets Ratio % Annual': 'NA',
            'ROE % Annual': 'NA',
            'ROA % Annual': 'NA',
            'FCF % Annual': 'NA',
            'P/E Ratio in start_date': 'NA',
            'Beta in start_date': 'NA',
            'Annual Return %': 'NA'
        })

# Convert the list to a DataFrame
df = pd.DataFrame(companies)

# Save the DataFrame to a CSV file
df.to_csv('nasdaq100_annual_metrics.csv', index=False)

print("NASDAQ-100 metrics saved to 'nasdaq100_annual_metrics.csv'.")
