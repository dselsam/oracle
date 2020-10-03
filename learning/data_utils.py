from itertools import chain
from dataclasses import dataclass
from typing import List
from torch.utils.data import Dataset
from torchtext.vocab import build_vocab_from_iterator
import torch

@dataclass
class Embeddable:
    text: str = None
    val: float = None

@dataclass
class ChoicePoint:
    snapshot: Embeddable
    choices: List[Embeddable]


def collate_fn(batch: List[dict], pad_token_id: int):
    num_samples = len(batch)
    batch_dict = {}
    for data_point in batch:
        for key, val in data_point.items():
            if key in batch_dict:
                batch_dict[key].append(val)
            else:
                batch_dict[key] = [val]
    max_snapshot_len = max(batch_dict['snapshot_len'])
    max_num_choices = max([len(choices_ids_i) for choices_ids_i in batch_dict['choices_ids']])
    max_choices_len = max(chain.from_iterable(batch_dict['choices_lens']))
    snapshot_ids = torch.LongTensor([seq + [pad_token_id] * (max_snapshot_len - len(seq))
                                     for seq in batch_dict['snapshot_ids']])
    snapshot_pad_mask = (snapshot_ids == pad_token_id)
    choices_ids = []
    for choices_ids_i in batch_dict['choices_ids']:
        num_choices = len(choices_ids_i)
        choices_ids_i_padded = [seq + [pad_token_id] * (max_choices_len - len(seq))
                                for seq in choices_ids_i]
        choices_ids_i_padded += [[pad_token_id] * max_choices_len] * (max_num_choices - num_choices)
        choices_ids.append(choices_ids_i_padded)
    choices_ids = torch.LongTensor(choices_ids)
    choices_pad_mask = (choices_ids == pad_token_id)
    assert list(snapshot_ids.shape) == [num_samples, max_snapshot_len]
    assert list(choices_ids.shape) == [num_samples, max_num_choices, max_choices_len]
    return {'snapshot_ids': snapshot_ids,
            'snapshot_pad_mask': snapshot_pad_mask,
            'choices_ids': choices_ids,
            'choices_pad_mask': choices_pad_mask,
            }

class EmbeddableDataset(Dataset):
    def __init__(self, text_file_path, tokenizer=str.split):
        self.data = []
        self.vocab = None
        self.tokenizer = tokenizer
        self.process_text_file(text_file_path)

    def __getitem__(self, idx: int):
        data_point = self.data[idx]
        snapshot_ids, snapshot_len = self.convert_text_to_ids(data_point.snapshot.text)
        choices_ids, choices_lens = map(list, zip(*[self.convert_text_to_ids(choice.text)
                                                    for choice in data_point.choices]))
        return {'snapshot_ids': snapshot_ids,
                'snapshot_len': snapshot_len,
                'choices_ids': choices_ids,
                'choices_lens': choices_lens
                }

    def process_text_file(self, file_path, save_vocab=False):
        tokens = [['<unk>', '<pad>', '<sos>', '<eos>']]
        with open(file_path) as f:
            snapshot = None
            choices = None
            while True:
                l = f.readline()
                if not l:
                    break
                val, embeddable_type, text = l.split(' ', maxsplit=2)
                tokens.append(iter(self.tokenizer(text)))
                embeddable = Embeddable(text=text.strip(), val=eval(val))
                if embeddable_type == 'SNAPSHOT':
                    if snapshot is not None:
                        self.data.append(ChoicePoint(snapshot=snapshot, choices=choices))
                    snapshot = embeddable
                    choices = []
                elif embeddable_type == 'CHOICE':
                    choices.append(embeddable)
                else:
                    raise ValueError(f'{embeddable_type} is not a valid embeddable type.')
            self.data.append(ChoicePoint(snapshot=snapshot, choices=choices))
        self.vocab = build_vocab_from_iterator(tokens)

    def convert_text_to_ids(self, text):
        tokens = self.tokenizer(text)
        ids = [self.vocab[token] for token in tokens]
        return ids, len(ids)

    def convert_ids_to_text(self, ids: list):
        return ' '.join(self.vocab.itos[idx] for idx in ids)

    def __len__(self):
        return len(self.data)

    @property
    def pad_token_id(self):
        return self.vocab.stoi['<pad>']
