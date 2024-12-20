import yfinance as yf

# Example ticker
ticker = "MSFT"

# Create a Ticker object
stock = yf.Ticker(ticker)

# Retrieve cashflow
cashflow = stock.cashflow

# Check available values
if cashflow is not None:
    print("Columns (Periods):")
    print(cashflow.columns)
    print("\nRows (Metrics):")
    print(cashflow.index)
else:
    print("No cashflow data available for this ticker.")
