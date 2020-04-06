import pandas as pd
import numpy as np

wiki = "https://en.wikipedia.org/wiki/List_of_National_Historic_Landmarks_in_New_York_City"

table = pd.read_html(wiki)[0]
lm_df = pd.DataFrame(table)[["Landmark name", "Location"]]

# https://www.geeksforgeeks.org/python-pandas-split-strings-into-two-list-columns-using-str-split/
# new data frame with split value columns
new = lm_df["Location"].str.split(r' (?<!^)([0-9]+)', n = 1, expand = True)
newnew = lm_df["Location"].str.split("/ ", n = 1, expand = True)
lat = newnew[1].str.split(u'°', n = 2, expand = True)
long = lat[1].str.split("N ", n = 1, expand = True)

lm_temp = pd.DataFrame()
lm_temp["Lat"] = lat[0]
lm_temp["Long"] = long[1]

# print(lm_temp.dtypes)

# make latitude a float
for i in range(len(lm_temp["Lat"])):
    lm_temp["Lat"][i] = float(lm_temp["Lat"][i].replace('\ufeff', ''))

lm_temp["Lat"] = lm_temp["Lat"].astype(dtype=np.float64)
lm_temp["Long"] = lm_temp["Long"].astype(dtype=np.float64)

lm_df["Location name"] = new[0]
lm_df["Lat"] = lm_temp["Lat"]
lm_df["Long"] = lm_temp["Long"]

# Dropping old Location column
lm_df.drop(columns =["Location"], inplace = True)

# print(lm_df[["Landmark name"]])
# print(lm_df[["Location name"]])
# print(lm_df[["Lat"]])
# print(lm_df[["Long"]])

lm_df.to_csv(r'lm_df.csv', index = None, header=True)
