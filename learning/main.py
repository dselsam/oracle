from functools import partial
from torch.utils.data import DataLoader
from data_utils import EmbeddableDataset, collate_fn
from encoders import TextEncoder
from config import TextEncoderConfig, TrainConfig
import torch


def set_device():
    if torch.cuda.is_available():
        return 'cuda'
    return 'cpu'


def main():
    text_file = './dummy_data.txt'
    device = set_device()
    dataset = EmbeddableDataset(text_file)
    dataloader = DataLoader(dataset,
                            shuffle=True,
                            batch_size=TrainConfig.batch_size,
                            collate_fn=partial(collate_fn, pad_token_id=dataset.pad_token_id),
                            )

    text_enc = TextEncoder(vocab_size=len(dataset.vocab),
                           embed_dim=TextEncoderConfig.embed_dim,
                           feedforward_dim=TextEncoderConfig.feedforward_dim,
                           snapshot_num_heads=TextEncoderConfig.snapshot_num_heads,
                           snapshot_num_layers=TextEncoderConfig.snapshot_num_layers,
                           choice_num_heads=TextEncoderConfig.choice_num_heads,
                           choice_num_layers=TextEncoderConfig.choice_num_layers,
                           dropout=TextEncoderConfig.dropout,
                           pad_token_id=dataset.pad_token_id
                           ).to(device)

    for _ in range(TrainConfig.num_epochs):
        for batch in iter(dataloader):
            snapshot_ids, snapshot_pad_mask, choices_ids, choices_pad_mask = [t.to(device) for t in batch.values()]
            snapshot_embed, choices_embed = text_enc(snapshot_ids, snapshot_pad_mask, choices_ids, choices_pad_mask)


if __name__ == '__main__':
    main()
