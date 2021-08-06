import matplotlib.pyplot as plt
import pandas

df = pandas.read_csv("gpu-stats.csv")
fig, axes = plt.subplots(nrows=2, ncols=1)

df["time"][0] = 0
df.plot(
    x="time", y=["mem"], figsize=(80, 10), legend=True, title="Memory usage", ax=axes[0]
)

df["util_rolling"] = df.loc[:, "util"].rolling(window=60).mean()
df.plot(
    x="time",
    y=["util", "util_rolling"],
    figsize=(80, 10),
    legend=True,
    title="GPU utilization",
    ax=axes[1],
)

plt.savefig("gpu-stats.png")
