"""
A command line tool to get an experiment data.csv file and a stimuli
generation data.json file and output a new .csv file with new columns
for analysis.
"""

import argparse
from collections import defaultdict, Iterable
import json

import numpy as np
import pandas as pd


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('datafile',
                        help='data.csv file for the experiment')
    parser.add_argument('stimulifile',
                        help='data.json file for the stimuli generation')
    parser.add_argument('--outfile', '-o',
                        help='annotated output csv file')
    return parser.parse_args()

def note_text(note):
    texts = [
        'C',
        'C#',
        'D',
        'D#',
        'E',
        'F',
        'F#',
        'G',
        'G#',
        'A',
        'B',  # Notice that A# is skipped
    ]
    return texts[note]


def flatten(iterable):
    for item in iterable:
        if isinstance(item, Iterable):
            yield from flatten(item)
        else:
            yield item


def word_text(word):
    return ''.join(note_text(note) for note in word)


def get_answer_keys(stimuli_data):
    """
    Return a dict where keys are (language, test) and values are lists the
    correct answers.
    """
    answer_keys = defaultdict(list)
    for lang_num, language in enumerate(stimuli_data['languages'], start=1):
        assert language['num'] == lang_num
        for test_num, trials in enumerate(stimuli_data['trials'], start=1):
            for x, y in trials:
                if x in language['words']:
                    answer_keys[(lang_num, test_num)].append(1)
                else:
                    assert y in language['words']
                    answer_keys[(lang_num, test_num)].append(2)
    return answer_keys


def get_expected(df, stimuli_data):
    """
    Get the expected answer as 1 or 2.
    """
    answer_keys = get_answer_keys(stimuli_data)
    return df.apply(
        lambda row: answer_keys[(row.language, row.test)][row.trial - 1],
        axis=1
    )


def get_correct_word(df, stimuli_data):
    """
    Return the correct words.
    """
    tests = stimuli_data['trials']
    return df.apply(
        lambda row: tests[row.test - 1][row.trial - 1][row.expected - 1],
        axis=1
    )


def calculate_trans_probs(story):
    """
    Calculate a 11 * 11 matrix of transitional probabilities in the "tone story".
    Cell [i, j] in the matrix is the probability of transitioning to note j after
    note i.
    """
    frequency_matrix = np.zeros((11, 11))
    flat_story = list(flatten(story))
    for x, y in zip(flat_story, flat_story[1:]):
        frequency_matrix[x, y] += 1
    # Normalized rows
    trans_probs = frequency_matrix / frequency_matrix.sum(axis=1)[:, np.newaxis]
    return np.nan_to_num(trans_probs)


def word_trans_prob(word, trans_probs):
    """
    Calculate the average transitional probabilities of a word.
    """
    return (trans_probs[word[0], word[1]] + trans_probs[word[1], word[2]]) / 2


def get_correct_word_trans_prob(df, stimuli_data):
    """
    Return the average transitional probabilities of the correct word.
    """
    trans_probs = {}
    for lang_num, language in enumerate(stimuli_data['languages'], start=1):
        trans_probs[lang_num] = calculate_trans_probs(language['story'])
    return df.apply(
        lambda row: word_trans_prob(row.correct_word, trans_probs[row.language]),
        axis=1
    )


def main():
    args = parse_args()
    df = pd.read_csv(args.datafile)
    with open(args.stimulifile) as f:
        stimuli_data = json.load(f)

    df['expected'] = get_expected(df, stimuli_data)
    df['correct'] = (df.answer == df.expected).astype('int')
    df['correct_word'] = get_correct_word(df, stimuli_data)
    df['correct_word_trans_prob'] = get_correct_word_trans_prob(df, stimuli_data)
    # Override 'correct_word' with its string representation
    df['correct_word'] = df.correct_word.apply(word_text)

    if args.outfile:
        df.to_csv(args.outfile, index=False)
    else:
        print(df)


if __name__ == '__main__':
    main()
