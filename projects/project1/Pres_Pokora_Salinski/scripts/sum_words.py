"""
Scipt sums number of words said by characters (we exclude characters that have over 500 words)
and returns name of the character, its gender and number of words said in all movies.
"""

import logging
import pandas as pd
import os

THRESHOLD = 500  # threshold made to exclude main characters

# Set working directory to current folder
os.chdir(os.path.dirname(os.path.abspath(__file__)))


def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(message)s")

    characters = pd.read_csv(
        "../data/Harry_Potter_Movies/Characters.csv", encoding="latin1"
    )
    dialogues = pd.read_csv(
        "../data/Harry_Potter_Movies/Dialogue.csv", encoding="latin1"
    )
    logging.info("Data loaded")

    merged_df = dialogues.merge(characters, on="Character ID", how="left")
    merged_df["Dialogue_length"] = merged_df["Dialogue"].str.split(" ").apply(len)
    logging.info("Data merged")

    character_sum_words = (
        merged_df.groupby("Character Name")
        .aggregate({"Dialogue_length": "sum"})
        .reset_index()
    )
    character_sum_words = character_sum_words.merge(
        characters, on="Character Name", how="left"
    )[["Character Name", "Gender", "Dialogue_length"]]
    character_sum_words = character_sum_words[
        character_sum_words["Dialogue_length"] <= THRESHOLD
    ].reset_index(drop=True)
    logging.info("Data aggregated")

    # Change 'Human' to 'Male' in Gender column
    character_sum_words["Gender"] = character_sum_words["Gender"] = character_sum_words[
        "Gender"
    ].replace("Human", "Male")

    character_sum_words.to_csv("../data/plots_data/sum_words.csv", index=False)
    logging.info("Data saved")


if __name__ == "__main__":
    main()
