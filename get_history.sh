#!/usr/bin/env bash
rm ing.db
git-history file ing.db ing-forecasts_3.csv --import csv --import io --convert '
fp = io.StringIO(content.decode("utf-8"))
r = csv.DictReader(fp)
x = [i for i in r if i[r.fieldnames[1]] == "UK"][0]
return [dict(f="ing", t=k, v=v) for k, v in x.items() if "." in v and v.replace(".", "").isnumeric()]
'
