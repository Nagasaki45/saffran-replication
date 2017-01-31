"""
A command line tool to get an experiment data.csv file and a stimuli
generation data.json file and output a .csv file with new column for answer
correctness (1 if the answer is correct or 0 otherwise).
"""

import argparse
from collections import defaultdict
import json

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


def get_answer_keys(stimulifile):
    """
    Return a dict where keys are (language, test) and values are lists the
    correct answers.
    """
    with open(stimulifile) as f:
        stimuli_data = json.load(f)
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


def get_expected(row, answer_keys):
    return answer_keys[(row.language, row.test)][row.trial - 1]


def main():
    args = parse_args()
    answer_keys = get_answer_keys(args.stimulifile)
    df = pd.read_csv(args.datafile)
    df['expected'] = df.apply(lambda row: get_expected(row, answer_keys), axis=1)
    df['correct'] = (df.expected == df.answer).astype('int')
    if args.outfile:
        df.to_csv(args.outfile, index=False)
    else:
        print(df)


if __name__ == '__main__':
    main()
