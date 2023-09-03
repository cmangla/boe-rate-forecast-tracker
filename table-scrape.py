#!/usr/bin/env python3
import pandas as pd

for i in ('ing-forecasts.html', 'scotiabank-forecast-snapshot.html'):
    dfs = pd.read_html(i)
    for j in range(len(dfs)):
        fn = i.removesuffix(".html") + "_" + str(j) + ".csv"
        dfs[j].dropna(how='all').to_csv(fn)
