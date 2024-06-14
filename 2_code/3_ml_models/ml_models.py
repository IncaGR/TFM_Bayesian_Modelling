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

# add max depth to avoid overfitting
# learning rate or eta is the step size shrinkage used to prevent overfitting
xgb_m = xgb.XGBRegressor(max_depth = 3, learning_rate = 0.3)

xgb_m.fit(X_train, y_train)

predictions = xgb_m.predict(X_test)

print(r2_score(y_test, predictions))

# add more variables to the models.

# data = pd.DataFrame(np.arange(12).reshape((4,3)), columns=['a', 'b', 'c'])
# # label = pd.DataFrame(np.random.randint(2, size=4))
# dtrain = xgb.DMatrix(data)

# # print(label)
# print(data)
# print(dtrain)

# print(data.index)

# print(data.loc[])

data_c = data.copy()
data_c = data_c[['price',
                'rooms2',
                'square_mt',
                'wc2',
                'terraza_balcon',
                'amueblado',
                'asc',
                'calef',
                'aire',
                'exterior',
                'casa',
                'estudio',
                'lujo',
            #    'distrito2',
            #    'regex_barris'
                           ]]

# data_c['distrito2'] = data_c['distrito2'].astype('category')
# data_c['regex_barris'] = data_c['regex_barris'].astype('category')

train, test = train_test_split(data_c, test_size=0.2, random_state=42)

dtrain = xgb.DMatrix(train.loc[:,'rooms2':], label=train['price'], enable_categorical=True)
dtest = xgb.DMatrix(test.loc[:,'rooms2':], label=test['price'], enable_categorical=True)

xgb_m = xgb.XGBRegressor(max_depth = 3, learning_rate = 0.3)

xgb_m.fit(dtrain.get_data(), dtrain.get_label())


predictions = xgb_m.predict(dtest.get_data())

print(r2_score(dtest.get_label(), predictions))

# Plot outputs
plt.scatter(train['square_mt'], train.price, color="black")
plt.scatter(test['square_mt'], predictions, color="blue")

plt.xticks(())
plt.yticks(())

# plt.show()

# introducing K-fold cross validation

