from transformers import AutoTokenizer, AutoModel
import torch
import gc


# garbage collection
def clear_gpu_memory():
    torch.cuda.empty_cache()
    torch.cuda.synchronize()
    gc.collect()

# delete model
def del_model(model):
    del model

# get device
def get_device():
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    return device
  
def load_model_and_tokenizer(model_path, device="cpu"):
    model = AutoModel.from_pretrained(model_path, local_files_only=True)
    tokenizer = AutoTokenizer.from_pretrained(model_path, local_files_only=True)
    model.to(device)
    return model, tokenizer

# Load model
def load_model(model_path, device="cpu"):
    if device.type == 'cuda':
        clear_gpu_memory()
    model = AutoModel.from_pretrained(model_path, 
              trust_remote_code=True,
              local_files_only=True,
              token="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")
    model.to(device)
    gc.collect()
    return model

# Load tokenizer
def load_tokenizer(model_path):
    tokenizer = AutoTokenizer.from_pretrained(model_path, local_files_only=True, trust_remote_code=True)
    gc.collect()
    return tokenizer

# Function to generate embeddings for a text chunk
def get_chunk_embedding(chunk, model, tokenizer, max_length, device):
    inputs = tokenizer(chunk, return_tensors="pt", truncation=True, padding=True, max_length=max_length)
    inputs = {key: value.to(device) for key, value in inputs.items()}
    with torch.no_grad():
        outputs = model(**inputs)
    embd_chunks = outputs.last_hidden_state.mean(dim=1)
    return embd_chunks

# Function to handle long text with chunking and stride
def generate_embeddings(text, model, tokenizer, max_length, stride, device):
    max_length = int(max_length)
    stride = int(stride)
    tokens = tokenizer.tokenize(text)
    chunks = [" ".join(tokens[i:i+max_length]) for i in range(0, len(tokens), stride)]
    chunk_embeddings = [get_chunk_embedding(chunk, model, tokenizer, max_length, device) for chunk in chunks]
    embeddings = torch.stack(chunk_embeddings).mean(dim=0)
    gc.collect()
    return embeddings.cpu().flatten().tolist()
