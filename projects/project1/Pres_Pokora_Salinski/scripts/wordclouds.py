"""
Script to generate wordclouds for chosen characters.
"""

import logging
import pandas as pd
import matplotlib.pyplot as plt
import wordcloud
import os

# Set working directory to current folder
os.chdir(os.path.dirname(os.path.abspath(__file__)))

CHARACTERS_TO_PLOT = [
    "Ron Weasley",
    "Hermione Granger",
    "Draco Malfoy",
    "Severus Snape",
]
IDX = [2, 3, 12, 6]


def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(message)s")

    dialogues = pd.read_csv(
        "../data/Harry_Potter_Movies/Dialogue.csv", encoding="latin1"
    )
    logging.info("Data loaded")

    for i, character in enumerate(CHARACTERS_TO_PLOT):
        text = " ".join(
            dial for dial in dialogues["Dialogue"][dialogues["Character ID"] == IDX[i]]
        )
        word_cloud = wordcloud.WordCloud(
            max_words=40, background_color="black", random_state=1
        ).generate(text)

        color = "YlGn" if i in [2, 3] else "autumn"

        plt.figure(figsize=(7, 7))
        plt.imshow(word_cloud.recolor(colormap=color), interpolation="bilinear")
        plt.title(
            character,
            fontdict={"fontsize": 16, "fontweight": "bold"},
            pad=20,
            color="white",
        )
        plt.axis("off")
        plt.savefig(f"../plots/{character.replace(' ', '_')}_wordcloud.png",
                    bbox_inches='tight', facecolor='black')
        logging.info(f"Wordcloud for {character} saved")


if __name__ == "__main__":
    main()
