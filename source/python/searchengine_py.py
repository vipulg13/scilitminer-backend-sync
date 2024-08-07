from ast import operator
from dataclasses import fields
import json
from tokenize import String
from elasticsearch import Elasticsearch
from elasticsearch_dsl import Search, Q
import collections


def getQuery(qryObj):
    qryDict = collections.defaultdict(list)
    qryObj = json.loads(qryObj)
    for i, q in enumerate(qryObj):
        qryName = "match_phrase" if q["phrase"] else "match"
        if q["field"] == "captions.description":
            name = "name" + str(i)
            if q["phrase"] and q["figuretype"] is not None:
                qryDict[q["type"]].append(
                    Q(
                        "nested",
                        path="captions",
                        query=Q(
                            "bool",
                            must=Q(qryName, **{q["field"]: {"query": q["keyword"]}}),
                            filter=Q("terms", **{"captions.tag": q["figuretype"]}),
                        ),
                        inner_hits={
                            "name": name,
                            "highlight": {"fields": {"captions.path": {}}},
                            "size": 20,
                        },
                        score_mode="sum",
                    )
                )
            elif q["phrase"] and q["figuretype"] is None:
                qryDict[q["type"]].append(
                    Q(
                        "nested",
                        path="captions",
                        query=Q(qryName, **{q["field"]: {"query": q["keyword"]}}),
                        inner_hits={
                            "name": name,
                            "highlight": {"fields": {"captions.path": {}}},
                            "size": 20,
                        },
                        score_mode="sum",
                    )
                )
            elif not q["phrase"] and q["figuretype"] is not None:
                qryDict[q["type"]].append(
                    Q(
                        "nested",
                        path="captions",
                        query=Q(
                            "bool",
                            must=Q(
                                qryName,
                                **{
                                    q["field"]: {
                                        "query": q["keyword"],
                                        "operator": q["operator"],
                                    }
                                },
                            ),
                            filter=Q("terms", **{"captions.tag": q["figuretype"]}),
                        ),
                        inner_hits={
                            "name": name,
                            "highlight": {"fields": {"captions.path": {}}},
                            "size": 20,
                        },
                        score_mode="sum",
                    )
                )
            else:
                qryDict[q["type"]].append(
                    Q(
                        "nested",
                        path="captions",
                        query=Q(
                            qryName,
                            **{
                                q["field"]: {
                                    "query": q["keyword"],
                                    "operator": q["operator"],
                                }
                            },
                        ),
                        inner_hits={
                            "name": name,
                            "highlight": {"fields": {"captions.path": {}}},
                            "size": 20,
                        },
                        score_mode="sum",
                    )
                )
        elif q["field"] == "all":
            qryDict[q["type"]].append(
                Q(
                    "combined_fields",
                    query=q["keyword"],
                    fields=["title", "abstract", "body", "captions", "references"],
                    operator=q["operator"],
                )
            )
        elif q["field"] in ["title", "abstract", "body", "references"]:
            if q["phrase"]:
                qryDict[q["type"]].append(
                    Q(qryName, **{q["field"]: {"query": q["keyword"]}},)
                )
            else:
                qryDict[q["type"]].append(
                    Q(
                        qryName,
                        **{
                            q["field"]: {
                                "query": q["keyword"],
                                "operator": q["operator"],
                            }
                        },
                    )
                )
        else:
            qryDict[q["type"]].append(Q("term", **{q["field"]: q["keyword"]}))

    qry = Q(
        "bool", must=qryDict["must"], should=qryDict["should"], must_not=qryDict["must_not"], filter=qryDict["filter"]
    )
    return qry


def getResults(resp):
    res = []
    hits = resp.hits
    if len(hits) == 0:
        return json.dumps({})
    else:
        for hit in hits:
            doc_dict = collections.defaultdict(list)
            doc_dict["_id"] = hit.meta.id
            doc_dict["score"] = hit.meta.score
            doc_dict["title"] = hit.title
            doc_dict["abstract"] = hit.abstract
            if "inner_hits" in hit.meta:
                aux_captions_dict = {}
                index_name_list = list(hit.meta.inner_hits)
                for index_name in index_name_list:
                    for caption_hits in hit.meta.inner_hits[index_name].hits:
                        pth = caption_hits.path if "path" in caption_hits else "null"
                        cap = caption_hits.description
                        if cap not in aux_captions_dict:
                            aux_captions_dict[cap] = pth
                            doc_dict["captions"].append({"path": pth, "caption": cap})
            res.append(doc_dict)
        resObj = json.dumps(res)
    return resObj


def searchEngine(qryObj, configObj):
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
          
    qry = getQuery(qryObj)
    s = Search(using=client, index=configObj["index_name"]).query(qry).extra(size=10000)
    resp = s.execute()
    resObj = getResults(resp)
    return resObj
  
  #[{"host": configObj["host"], "port": configObj["port"], "scheme": configObj["scheme"]}]
  #configObj["scheme"] + "://" + configObj["host"] + ":" + configObj["port"]
