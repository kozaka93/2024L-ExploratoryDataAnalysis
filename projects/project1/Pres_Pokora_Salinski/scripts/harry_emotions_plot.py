'''
Scripts plots participation of emotions in Harry's dialogues
by movie, using data from "harry_emotions_by_movies.csv".
'''

import logging
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os

sns.set(style="ticks")
plt.style.use("dark_background")

# Set working directory to current folder
os.chdir(os.path.dirname(os.path.abspath(__file__)))

def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(message)s")

    emotions_by_movies = pd.read_csv("../data/plots_data/harry_emotions_by_movies.csv")
    logging.info("Data loaded")

    plt.figure(figsize=(10, 7), facecolor='black')
    ax = sns.lineplot(data=emotions_by_movies, dashes=False,
                    markers=['o' for _ in range(6)],
                    palette=['#20A2FF', '#00FF00', '#FF007F', 'red', 'orange', 'yellow'],
                    linewidth=3)
    plt.title("Average participation of emotions in Harry's dialogues in each movie",
            fontdict={'fontsize': 18, 'fontweight': 'bold'}, pad=20,
            color='white', loc='left')
    plt.ylabel('Emotion score', fontdict={'fontsize': 12, 'fontweight': 'bold'},
            color='white')
    plt.xlabel('Movie Title', fontdict={'fontsize': 12, 'fontweight': 'bold'},
            color='white', family='monospace')
    plt.yticks(color='white')
    plt.xticks(color='white', rotation=30,
            ticks=range(len(emotions_by_movies['Movie Title'])),
            labels=emotions_by_movies['Movie Title'])
    ax.legend(title='Emotion', title_fontsize='16')
    sns.move_legend(ax, "upper left", bbox_to_anchor=(1, 1))
    plt.grid(axis='y', linestyle='--', alpha=0.5)
    plt.grid(axis='x', alpha=0)
    plt.savefig("../plots/harry_emotions_by_movies.png",
                bbox_inches='tight')
    logging.info("Plot saved")


if __name__ == "__main__":
    main()
