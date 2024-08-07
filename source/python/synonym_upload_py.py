from elasticsearch import Elasticsearch
from elasticsearch.client import SynonymsClient
import json

def putSynonymRule(synrule_id, synrule, configObj):
  
    """
    Update or create a document in Elasticsearch to store synonyms.

    Parameters:
    synObj (dict): The synonyms object to be indexed.
    configObj (dict): Configuration object containing Elasticsearch connection details.

    Returns:
    int: 0 if successful, 1 if failed.
    Exception: Returns the exception if one occurs.
    """
    synset_id = "domains_synonyms_set"
    
    try:
        conn_str = [
                     {
                       "scheme": configObj["scheme"], 
                       "host": configObj["host"], 
                       "port": configObj["port"]
                     }
                   ]
        if "ca_certs" in configObj:
            client = Elasticsearch(conn_str,
                                   basic_auth=(configObj["username"], configObj["password"]),
                                   ca_certs=configObj["ca_certs"])  
        else:
            client = Elasticsearch(conn_str,
                                   basic_auth=(configObj["username"], configObj["password"]))
        
        # Create a SynonymsClient object
        synonyms_client = SynonymsClient(client)
        
        response = synonyms_client.put_synonym_rule(set_id=synset_id, rule_id=synrule_id, synonyms=synrule)
        if response['result'] in ['created', 'updated']:
            return 0
        else:
            return 1
    except Exception as e:
        return str(e)
