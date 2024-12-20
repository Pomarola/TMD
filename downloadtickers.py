import yfinance as yf
import pandas as pd

# Create an empty DataFrame to store daily variations
df = pd.DataFrame()

# List of tickers 'YPF', 'PAM', 'BMA', 'BBAR',
tickers = ['QQQ', 'SPY', 'EEM', 'EWZ', 'XLF', 'BAC', 'JPM', 'BRK-B', 'NU', 'VALE', 'VIST', 'MELI', 'IEF', 'EMB', 'AAAU', 'BTC-USD', 'GGAL']

# Loop through each ticker
for ticker in tickers:
    # Download historical data
    data = yf.download(ticker, start="2020-01-01", end="2024-10-10")
    
    # Calculate daily price variation (percentage change)
    data['Price Variation'] = data['Adj Close'].pct_change()
    
    # Rename the 'Price Variation' column to the ticker symbol
    data = data[['Price Variation']].rename(columns={'Price Variation': ticker})
    
    # Concatenate the result with the main DataFrame, aligning on dates
    if df.empty:
        df = data
    else:
        df = pd.concat([df, data], axis=1)

# Save the result to a CSV file
df.to_csv('daily_price_variation.csv', index=True)

print("Data saved to 'daily_price_variation.csv'")