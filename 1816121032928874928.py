import pandas as pd
import numpy as np
import requests
from io import BytesIO
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.dates as mdates
from datetime import datetime
from matplotlib.ticker import FuncFormatter

# Step 1: Fetch the data
url = "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010768261+010768307"
response = requests.get(url)
data = pd.read_xml(BytesIO(response.content))

# Step 2: Convert to DataFrame and perform initial data manipulation
data = pd.DataFrame(data)
data['date'] = pd.to_datetime(data['TIME_PERIOD'] + "-01")
data = data[data['date'] >= "2017-05-01"]

# Step 3: Clean and transform data
data['OBS_VALUE'] = np.where((data['date'].dt.year == 2020) & (data['date'].dt.month.isin([3, 4, 5, 6])), np.nan, data['OBS_VALUE'].astype(float))
data['Naf2'] = data['TITLE_FR'].str.extract(r"(?<=- ).*(?= \(NAF)")

# Group and normalize
data = data.groupby('Naf2').apply(lambda group: group.assign(OBS_VALUE=100 * group['OBS_VALUE'] / group.loc[group['date'] == '2017-05-01', 'OBS_VALUE'].values[0]))
data.reset_index(drop=True, inplace=True)

# Step 4: Plotting
plt.figure(figsize=(1.25*6, 1.25*3.375))
sns.set(style="whitegrid")
palette = sns.color_palette("viridis", len(data['Naf2'].unique()))

# Line plot
sns.lineplot(data=data, x='date', y='OBS_VALUE', hue='Naf2', palette=palette)

# Add labels
last_values = data.loc[data.groupby('Naf2')['date'].idxmax()]
for line in last_values.itertuples():
    plt.text(line.date, line.OBS_VALUE, f"{line.OBS_VALUE/100-1:.1%}", horizontalalignment='left', size='small', color=line.Naf2)

# Add horizontal line at y=100
plt.axhline(y=100, color='grey', linestyle='--')

# Highlight period of interest
plt.axvspan(pd.to_datetime("2020-02-01"), pd.to_datetime("2020-07-01"), color=sns.color_palette("viridis", 4)[3], alpha=0.2)

# Formatting the x-axis
plt.gca().xaxis.set_major_locator(mdates.YearLocator())
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
plt.xticks(rotation=45)

# Formatting the y-axis
plt.gca().yaxis.set_major_formatter(FuncFormatter(lambda y, _: f"{y-100:.1%}"))

# Title and labels
plt.title("Indice de Production Industrielle vs. Mai 2017\nhors Mars-Juin 2020")
plt.xlabel("")
plt.ylabel("")

# Adjust legend
plt.legend(title='', loc='upper left')

# Save plot
plt.savefig("1816121032928874928.png", dpi=300, bbox_inches='tight', facecolor='white')
plt.savefig("1816121032928874928.pdf", dpi=300, bbox_inches='tight')

plt.show()
