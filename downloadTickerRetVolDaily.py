import yfinance as yf
import pandas as pd
import numpy as np
from datetime import datetime

tickers = [
    # ARGENTINA
    'GGAL',
    'YPF',
    'BMA',
    'BBAR',
    'SUPV',
    'PAM',
    'CEPU',
    'TX',
    'EDN',
    'CRESY',
    'TEO',
    'TGS',
    'LOMA',
    'MELI',
    'VIST',

    # BRAZIL
    'PBR',
    'NU',
    'ITUB',
    'VALE',
    'BBD',

    # CHINA
    'BABA',
    'TME',
    'JD',
    'BIDU',
    'PDD',
    'ZTO',

    # JAPAN
    'SONY',
    'TM',
    'MUFG',

    # USA
    'AAPL',
    'MSFT',
    'GOOGL',
    'AMZN',
    'TSLA',
    'META',
    'NVDA',
    'PYPL',
    'ADBE',
    'CSCO',
    'IBM',
    'ORCL',
    'QCOM',
    'AMD',
    'NFLX',
    'JPM',
    'GS',
    'C',
    'BAC',
    'WFC',
    'JNJ',
    'PFE',
    'GILD',
    'LLY',
    'KO',
    'PEP',
    'WMT',
]

# Initialize an empty list to store results
results = []

# Loop through each ticker and gather data
for ticker in tickers:
    # Download historical data for the past 5 years
    data = yf.Ticker(ticker).info

    print(ticker)

    trailing_pe = data['trailingPE']
    forward_pe = data['forwardPE']
    dividend_yield = data.get("dividendYield", 0)
    avg_volume = data['averageVolume']
    market_cap = data['marketCap']
    price_to_book = data['priceToBook']
    trailing_eps = data['trailingEps']
    forward_eps = data['forwardEps']
    beta = data.get("beta", 1)

    results.append({
        'ticker': ticker,
        'trailing_pe': trailing_pe,
        'forward_pe': forward_pe,
        'dividend_yield': dividend_yield,
        'avg_volume': avg_volume,
        'market_cap': market_cap,
        'price_to_book': price_to_book,
        'trailing_eps': trailing_eps,
        'forward_eps': forward_eps,
        'beta': beta
    })

# Create DataFrame with results
df = pd.DataFrame(results, columns=['ticker', 'trailing_pe', 'forward_pe', 'dividend_yield', 'avg_volume', 'market_cap', 'price_to_book', 'trailing_eps', 'forward_eps', 'beta'])

# Save the DataFrame to a CSV file
df.to_csv('metrics_stocks.csv', index=False)

print("CSV file created successfully.")
