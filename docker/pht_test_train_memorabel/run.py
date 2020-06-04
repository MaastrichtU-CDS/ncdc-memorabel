import os
import pandas as pd
import json
import numpy as np


database_uri = os.getenv('DATABASE_URI','no_database_uri')

test = pd.read_csv(database_uri)


column_names = test.columns
summary = test.describe()
stats = summary.to_json()
# cols = {}
# for col in summary.keys():
#     values = summary[col]
    # cols[col] = {
    #     "count": values["count"],
    #     "mean": values["mean"],
    #     "std": values["std"],
    #     "min": values["min"],
    #     "q1": values["25%"],
    #     "median": values["50%"],
    #     "q3": values["75%"],
    #     "max": values["max"]
    # }
#print(test['Age'].mean())

#test = test.set_index(['Age'], inplace = True)
#print(test["Age"]

result = {
    'Number of columns': len(column_names),
    'Number of rows': len(test),
    'statistics': stats
}
# # Write output to file
with open('output.txt', 'w') as f:
    f.write(json.dumps(result))