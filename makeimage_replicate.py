import os
import torch
import open_clip
from PIL import Image
import numpy as np
import csv


img_dir = "C:/Users/vachu/OneDrive/csproject/images/val2014"
out_dir = "C:/Users/vachu/OneDrive/csproject/imageembed"

lf1 = os.listdir(img_dir)

device = "cuda"

model, _, preprocess = open_clip.create_model_and_transforms('ViT-B-32', pretrained='laion2b_s34b_b79k', device=device)

img_files = lf1
total_imgs = len(img_files)

all_fnames = []
all_embeds = []

for i, fname in enumerate(img_files, start=1):
    img_path = os.path.join(img_dir, fname)
    image = Image.open(img_path).convert("RGB")
    image_tensor = preprocess(image).unsqueeze(0).to(device)
    with torch.no_grad():
        img_feat = model.encode_image(image_tensor).cpu().squeeze(0)

    all_fnames.append(fname)
    all_embeds.append(img_feat.numpy())

embed_dim = all_embeds[0].shape[0]
header = ["filename"] + [f"dim_{j+1}" for j in range(embed_dim)]

out_csv = os.path.join(out_dir, "clip_embeddings_vitb32_val2014.csv")
with open(out_csv, "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(header)
    for fname, emb in zip(all_fnames, all_embeds):
        writer.writerow([fname] + emb.tolist())
