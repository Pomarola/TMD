import yfinance as yf
import pandas as pd
import numpy as np
from datetime import datetime

tickers = [
    #ARGENTINA
    'GGAL',
    'YPF',
    'BMA',
    'BBAR',
    'SUPV',
    # 'PAM',
    'CEPU',
    'EDN',
    'TGS',
    'VIST',

    # #BRAZIL
    # 'PBR',
    # 'NU',
    # 'ITUB',
    # 'VALE',
    # 'BBD',

    #CHINA
    'BABA',
    'JD',
    'BIDU',

    #JAPAN
    'SONY',
    'TM',
    'MUFG',

    #USA TECH
    'AAPL',
    'MSFT',
    'GOOGL',
    'AMZN',
    'META',
    'NVDA',
    'NFLX',

    #USA FINANCIALS
    'JPM',
    'GS',
    'C',
    'BAC',
    'WFC',

    #USA HEALTHCARE
    'JNJ',
    'PFE',
    'GILD',
    'LLY',

    #USA CONSUMER DISCRETIONARY
    'SBUX',
    'MCD',
    'KO',
    'PEP',
    'WMT',

    #GERMANY
    'SAP',
    'VOW3.DE',
    'BMW.DE',
    'SIE.DE',
    'DHL.DE',


    #INDEXES
    # 'SPY',
    # 'QQQ',
    # 'DIA',
    # 'IWM',
    # 'EEM',
    # 'EWZ',
    # 'FXI',
    # 'ARGT',
    # 'EWG',
    # 'EWJ',
    # 'MCHI',
    # 'IEF',
    # 'EMB', 
    # 'AAAU',
    # 'GLD',
    # 'SLV',

]

# Set the start and end date
end_date = datetime.now().date()
start_date = end_date.replace(year=end_date.year - 3)

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
