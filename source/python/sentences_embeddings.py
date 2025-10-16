from sentence_transformers import SentenceTransformer, util
from nltk.tokenize import sent_tokenize
from transformers import BartTokenizer, BartForConditionalGeneration
import os
import nltk
import torch
import numpy as np
from nltk.data import load
import concurrent.futures




def load_sentence_model(model_name="all-MiniLM-L12-v2", model_zoo_path="xxx/yyy"):
  model_path = os.path.join(model_zoo_path, model_name)
  sentence_transformer_model = SentenceTransformer(model_path)
  return sentence_transformer_model

def generate_sentence_embeddings(document, sentence_model, tokenizer_path, max_passage_count=3):
  nltk.data.path.append(tokenizer_path)
  sentences = sent_tokenize(document)
  data = {}
  data["sentences"] = sentences
  
  # Generate passages and embeddings for each passage count up to max_passage_count
  for passage_count in range(1, max_passage_count + 1):
      # Generate passages with sliding window based on passage_count
      passages = [
          " ".join(sentences[i:i + passage_count])
          for i in range(len(sentences) - passage_count + 1)
      ]
      data[f"embeddings_{passage_count}"] = sentence_model.encode(passages, convert_to_tensor=False)
  return data


def load_summarizer_model(model_path="xxx/yyy"):
  bart_model = BartForConditionalGeneration.from_pretrained(model_path)
  return bart_model

def load_summarizer_tokenizer(model_path="xxx/yyy"):
  bart_tokenizer = BartTokenizer.from_pretrained(model_path)
  return bart_tokenizer

def extract_relevant_sentences(documents, query, sentence_model, sentence_per_passage=3, top_k=3, similarity_threshold=0.30, adjacent=0, num_workers=1):
    top_k = int(top_k)
    adjacent = int(adjacent)
    sentence_per_passage = int(sentence_per_passage)
    num_workers = int(num_workers)
    
    all_relevant_sentences = {}

    # Encode query
    query_embedding = sentence_model.encode(query, convert_to_tensor=True).to('cpu')

    embeddings_key = f'embeddings_{sentence_per_passage}'
    all_embeddings = documents[embeddings_key]
    all_sentences = documents['sentences']
    all_ids = documents['_id']
    
    def process_document(idx):
        embeddings = torch.tensor(all_embeddings[idx]).float().to('cpu')
        sentences = all_sentences[idx]
        doc_id = all_ids[idx]

        # Compute similarities
        similarities = util.pytorch_cos_sim(query_embedding, embeddings).cpu().numpy().flatten()

        # Get top_k indices based on similarity threshold
        top_k_indices = np.argsort(similarities)[-top_k:][::-1] if len(similarities) > top_k else np.argsort(similarities)[::-1]

        # Use numpy to directly filter indices by similarity threshold
        relevant_indices = top_k_indices[similarities[top_k_indices] >= similarity_threshold]

        # Store relevant sentences for the current document
        selected_sentences = set()
        for i in relevant_indices:
          
            # Determine the start and end indices of the passage
            start_index = max(i - adjacent, 0)
            end_index = min(i + adjacent + sentence_per_passage, len(sentences))
            
            # Add the selected passage to the selected_sentences set
            selected_sentences.update(sentences[start_index:end_index])

        # Store sentences in original order
        return doc_id, sorted(selected_sentences, key=lambda x: sentences.index(x))

    # Determine the maximum number of workers
    max_possible_workers = os.cpu_count() 
    if num_workers is None or num_workers > max_possible_workers:
        num_workers = max_possible_workers
    
    # Use ThreadPoolExecutor with defined threads
    with concurrent.futures.ThreadPoolExecutor(max_workers=num_workers) as executor:
        futures = [executor.submit(process_document, idx) for idx in range(len(all_embeddings))]
        
        for future in concurrent.futures.as_completed(futures):
            doc_id, relevant_sentences = future.result()
            all_relevant_sentences[doc_id] = relevant_sentences

    return all_relevant_sentences



def extract_relevant_sentences_old(documents, query, sentence_model, sentence_per_passage=3, top_k=3, similarity_threshold=0.30, adjacent=0):
    top_k = int(top_k)
    adjacent = int(adjacent)
    sentence_per_passage = int(sentence_per_passage)
    
    all_relevant_sentences = {}

    # Encode query
    query_embedding = sentence_model.encode(query, convert_to_tensor=True).to('cpu')

    embeddings_key = f'embeddings_{sentence_per_passage}'
    all_embeddings = documents[embeddings_key]
    all_sentences = documents['sentences']
    all_ids = documents['_id']
    
    for idx, embeddings in enumerate(all_embeddings):
        
        embeddings = torch.tensor(embeddings).float().to('cpu')
        sentences = all_sentences[idx]
        doc_id = all_ids[idx]

        # Compute similarities
        similarities = util.pytorch_cos_sim(query_embedding, embeddings).cpu().numpy().flatten()

        # Get top_k indices based on similarity threshold
        top_k_indices = np.argsort(similarities)[-top_k:][::-1] if len(similarities) > top_k else np.argsort(similarities)[::-1]

        # Use numpy to directly filter indices by similarity threshold
        relevant_indices = top_k_indices[similarities[top_k_indices] >= similarity_threshold]

        # Store relevant sentences for the current document
        selected_sentences = set()
        for i in relevant_indices:
            # Determine the start and end indices of the passage
            start_index = max(i - adjacent, 0)
            end_index = min(i + adjacent + sentence_per_passage, len(sentences))
            
            # Add the selected passage to the selected_sentences set
            selected_sentences.update(sentences[start_index:end_index])

        # Store sentences in original order
        all_relevant_sentences[doc_id] = sorted(selected_sentences, key=lambda x: sentences.index(x))
        
        '''
        # Expand indices based on the 'adjacent' parameter
        expanded_indices = set()
        for i in relevant_indices:
            # Add the range of indices within the bounds of the document
            for adj in range(-adjacent, adjacent + 1):
                if 0 <= i + adj < len(sentences):
                    expanded_indices.add(i + adj)

        # Store sentences in original order
        all_relevant_sentences[doc_id] = [sentences[i] for i in sorted(expanded_indices)]
        '''
    return all_relevant_sentences


def summarize_chunk(chunk, summarizer_model, summarizer_tokenizer, max_length=1024):
    inputs = summarizer_tokenizer(chunk, return_tensors="pt", max_length=max_length, truncation=True)
    
    # Generate summary
    summary_ids = summarizer_model.generate(
        inputs['input_ids'], 
        max_length=max_length, 
        min_length=128, 
        length_penalty=2.0, 
        num_beams=4, 
        early_stopping=True
    )
    
    # Decode summary
    summary = summarizer_tokenizer.decode(summary_ids[0], skip_special_tokens=True)
    
    return summary

def summarize_iteratively(sentences, summarizer_model, summarizer_tokenizer, chunk_size=768, max_summary_length=1024, stride=0.5):
    words = sentences.split()
    stride_size = int(chunk_size * stride)

    # If the sentence length is within the final desired summary length, summarize directly
    if len(words) <= chunk_size:
        return summarize_chunk(sentences, summarizer_model, summarizer_tokenizer, max_length=max_summary_length)

    chunk_summaries = []

    # Process the text in overlapping chunks
    for i in range(0, len(words), chunk_size - stride_size):
        chunk = " ".join(words[i:i + chunk_size])
        chunk_summary = summarize_chunk(chunk=chunk, summarizer_model=summarizer_model, summarizer_tokenizer=summarizer_tokenizer, max_length=max_summary_length)
        chunk_summaries.append(chunk_summary)

    # Combine all chunk summaries into a single text
    combined_summary = " ".join(chunk_summaries)

    # If the combined summary length is too long, recursively summarize
    tokenized_summary = summarizer_tokenizer(combined_summary, return_tensors="pt", max_length=50000, truncation=True)
    current_length = tokenized_summary['input_ids'].shape[1]
    print(f"Current summary length in tokens: {current_length}")
    print(combined_summary)
    # Check if the combined summary exceeds the maximum length
    if current_length > max_summary_length:
        # Recursively call summarize_iteratively until we reach the desired length
        return summarize_iteratively(
            combined_summary, 
            summarizer_model, 
            summarizer_tokenizer, 
            chunk_size=chunk_size, 
            max_summary_length=max_summary_length, 
            stride=stride
        )

    return combined_summary


def combined_sentences(relevant_sentences):
  
    # Combine all relevant sentences for summarization
    combined_sentences = [sentence for sentences in relevant_sentences.values() for sentence in sentences]
    return combined_sentences

def combined_sentences_dict(relevant_sentences):
    # Create a dictionary where each source is tagged with a simplified key
    combined_dict = {}
    
    # Loop through each source and its list of sentences
    for idx, (source, sentences) in enumerate(relevant_sentences.items(), start=1):
        # Assign a unique tag for each source
        simplified_tag = f"source_{idx}"
        
        # Combine all sentences for the current source into a single block of text
        combined_text = " ".join(sentences)
        
        # Add the combined text to the dictionary with its simplified tag
        combined_dict[simplified_tag] = combined_text
    
    return combined_dict


