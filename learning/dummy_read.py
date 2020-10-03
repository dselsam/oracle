import pandas as pd
from dataclasses import dataclass
from typing import List
from torchtext.vocab import build_vocab_from_iterator

@dataclass
class Embeddable:
    text: str = None
    val: float = None


@dataclass
class ChoicePoint:
    snapshot: Embeddable
    choices: List[Embeddable]


filepath = './dummy_data.txt'
# with open(filepath, 'r') as f:
#     while True:
#         l = f.readline()
#         if not l:
#             print('Done')
#             break
#         else:
#             print(repr(l))
#
tokens = []
data = []

with open(filepath) as f:
    snapshot = None
    choice_list = None
    while True:
        x = f.readline()
        if not x:
            break
        val, embeddable_type, text = x.split(' ', maxsplit=2)
        print(val, embeddable_type, text, sep=', ')
        print(text.split())
        tokens.append(iter(text.split()))
        embeddable = Embeddable(text=text.strip(), val=eval(val))

        if embeddable_type == 'SNAPSHOT':
            if snapshot is not None:
                print('Time to append')
                data.append(ChoicePoint(snapshot=snapshot, choices=choices))
            snapshot = embeddable
            choices = []
        elif embeddable_type == 'CHOICE':
            choices.append(embeddable)
    data.append(ChoicePoint(snapshot=snapshot, choices=choices))

