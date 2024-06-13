import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt

data = pd.read_csv('../TFM/1_data/2_data_Idealista/2_clean/data_idealista_clean_2023-05-03.csv')
data.drop(columns=['Unnamed: 0'], inplace=True )

print(data.head())
print(data.columns)

X = data[['rooms2','square_mt','wc2']]
y = data['price']

# to numpy and train test split
print(X.shape)

X = X.to_numpy()
y = y.to_numpy()

print(X.shape,y.shape)


X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.33, random_state=42)

print(X_train.shape, X_test.shape, y_train.shape, y_test.shape)

# baseline model

lm = LinearRegression()

lm.fit(X_train, y_train)

print(lm.score(X_test,y_test))

predictions = lm.predict(X_test)

print('Coefficients: \n', lm.coef_)
print('Mean squared error: %.2f'
      % mean_squared_error(y_test, predictions))
print(r2_score(y_test, predictions))

# Plot outputs
plt.scatter(X_test[:,1], y_test, color="black")
plt.scatter(X_test[:,1], predictions, color="blue")

plt.xticks(())
plt.yticks(())

# plt.show()
# data[''] = lm.predict(X_test)

# Xgboost

import xgboost as xgb

xgb_m = xgb.XGBRegressor()

xgb_m.fit(X_train, y_train)

predictions = xgb_m.predict(X_test)

print(r2_score(y_test, predictions))