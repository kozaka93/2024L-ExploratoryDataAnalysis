"""
Script calculating emotions in Harry Potter dialogues.
It uses bert-base-uncased-emotion Transformer from Hugging Face.
"""

import logging
import pandas as pd
import numpy as np
from transformers import pipeline
import os

import warnings

warnings.filterwarnings("ignore")

# Set working directory to current folder
os.chdir(os.path.dirname(os.path.abspath(__file__)))


def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(message)s")

    transformer = transformer = pipeline(
        "text-classification",
        model="bhadresh-savani/bert-base-uncased-emotion",
        return_all_scores=True,
    )
    logging.info("Transformer loaded")

    dialogues = pd.read_csv(
        "../data/Harry_Potter_Movies/Dialogue.csv", encoding="latin1"
    )
    chapters = pd.read_csv(
        "../data/Harry_Potter_Movies/Chapters.csv", encoding="latin1"
    )
    movies = pd.read_csv("../data/Harry_Potter_Movies/Movies.csv", encoding="latin1")
    movies.columns = [
        "Movie ID",
        "Movie Title",
        "Release Year",
        "Runtime",
        "Budget",
        "Box Office",
    ]
    logging.info("Data loaded")

    dialogue_harry_df = dialogues[dialogues["Character ID"] == 1][
        ["Dialogue", "Chapter ID"]
    ].reset_index(drop=True)
    dialogue_harry_df = dialogue_harry_df.merge(chapters, on="Chapter ID", how="left")[
        ["Dialogue", "Movie ID", "Chapter ID"]
    ]

    emotions = transformer(dialogue_harry_df["Dialogue"].tolist())
    logging.info("Emotions calculated")

    for i, emotion in enumerate(
        ["sadness", "joy", "love", "anger", "fear", "surprise"]
    ):
        dialogue_harry_df[emotion] = np.round(
            [emotions[j][i]["score"] for j in range(len(emotions))], 3
        )

    dialogue_harry_df.columns = [
        "Dialogue",
        "Movie ID",
        "Chapter ID",
        "Sadness",
        "Joy",
        "Love",
        "Anger",
        "Fear",
        "Surprise",
    ]
    dialogue_harry_df = dialogue_harry_df.drop(
        ["Dialogue", "Chapter ID"], axis=1
    )  # Drop unnecessary columns

    emotions_by_movies = dialogue_harry_df.groupby("Movie ID").mean()[
        ["Sadness", "Joy", "Love", "Anger", "Fear", "Surprise"]
    ]
    emotions_by_movies = emotions_by_movies.merge(
        movies, on="Movie ID", how="left"
    ).drop(["Movie ID", "Release Year", "Runtime", "Budget", "Box Office"], axis=1)
    emotions_by_movies["Movie Title"] = emotions_by_movies["Movie Title"].str.replace(
        "Harry Potter and the ", ""
    )
    logging.info("Emotions by movies calculated")
    logging.info(emotions_by_movies)

    emotions_by_movies.to_csv(
        "../data/plots_data/harry_emotions_by_movies.csv", index=False
    )
    logging.info("Data saved")


if __name__ == "__main__":
    main()
