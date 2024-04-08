"""
Script plots distrubution of words said by characters splitted
by their gender. It uses data from sum_words.csv.
"""

import logging
import pandas as pd
import os
import matplotlib.pyplot as plt
import seaborn as sns

sns.set(style="ticks")
plt.style.use("dark_background")

# Set working directory to current folder
os.chdir(os.path.dirname(os.path.abspath(__file__)))


def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(message)s")

    data = pd.read_csv("../data/plots_data/sum_words.csv")
    logging.info("Data loaded")

    plt.figure(figsize=(7, 10), facecolor="black")
    ax = sns.boxplot(
        data=data,
        x="Gender",
        y="Dialogue_length",
        color="#30A3A3",
        linewidth=3,
        linecolor="white",
    )
    plt.title(
        "Words said by characters split by their gender\n (main characters excluded)",
        fontdict={"fontsize": 16, "fontweight": "bold"},
        pad=20,
        color="white",
    )
    plt.ylabel(
        "Number of words",
        fontdict={"fontsize": 12, "fontweight": "bold"},
        color="white",
    )
    plt.yticks(color="white")
    plt.xticks(color="white")
    plt.grid(axis="y", linestyle="--", alpha=0.5)
    plt.savefig("../plots/dist_words_by_gender.png")
    logging.info("Plot saved")


if __name__ == "__main__":
    main()
