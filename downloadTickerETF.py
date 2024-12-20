import yfinance as yf
import pandas as pd
import numpy as np
from datetime import datetime

tickers = [
    # XLF
    'XLF',
    'BAC',
    'JPM',
    'BRK-B',
    'V',
    'MA',
    'WFC',

    # XLV
    'XLV',
    'LLY',
    'UNH',
    'JNJ',
    'ABBV',
    'MRK',
    'TMO',

    # XLI
    'XLI',
    'GE',
    'CAT',
    'RTX',
    'UNP',
    'HON',

    # XLU
    'XLU',
    'NEE',
    'SO',
    'DUK',
    'AEP',
    'SRE',

    # XLP
    'XLP',
    'PG',
    'COST',
    'WMT',
    'KO',
    'PEP',
    'PM',

    # XLY
    'XLY',
    'AMZN',
    'HD',
    'MCD',
    'LOW',
    'BKNG',

    # XLB
    'XLB',
    'LIN',
    'SHW',
    'APD',
    'ECL',
    'NEM',

    # XLC
    'XLC',
    'META',
    'GOOGL',
    'DIS',
    'T',
    'VZ',

    # XLE
    'XLE',
    'XOM',
    'CVX',
    'COP',
    'SLB',
    'EOG',
    'WMB',

    # XLRE
    'XLRE',
    'PLD',
    'AMT',
    'EQIX',
    'WELL',
    'PSA',
    'O',

]

# Set the start and end date
end_date = datetime.now().date()
start_date = end_date.replace(year=end_date.year - 1)

# Initialize an empty list to store results
results = []

# Function to calculate annualized return and volatility
def calculate_metrics(ticker_data):
    # Calculate daily returns
    ticker_data['Daily Return'] = ticker_data['Adj Close'].pct_change()
    
    # Calculate average daily return
    avg_daily_return = np.mean(ticker_data['Daily Return'])
    
    # Calculate daily volatility (standard deviation of daily returns)
    daily_volatility = np.std(ticker_data['Daily Return'])
    
    # Annualize the daily return and daily volatility
    annualized_return = avg_daily_return * 252
    annualized_volatility = daily_volatility * np.sqrt(252)
    
    return annualized_return, annualized_volatility

for ticker in tickers:
    # Download historical data for the past 3 years
    data = yf.download(ticker, start=start_date, end=end_date)
    
    if not data.empty:
        # Calculate metrics
        annual_return, annual_volatility = calculate_metrics(data)
        
        # Append to results list
        results.append([ticker, annual_return, annual_volatility])

# Create DataFrame with results
df = pd.DataFrame(results, columns=['ticker', 'annual_avg_return', 'annual_avg_volatility'])

# Save the DataFrame to a CSV file
df.to_csv('annualized_metrics.csv', index=False)

print("CSV file created successfully.")
