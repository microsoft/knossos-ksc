# mypy: ignore-errors
from collections import defaultdict
from typing import Dict, Iterator, List, Union

import numpy as np
from sklearn.cluster import KMeans

from rlo import analytics
from rlo import utils
from rlo.search_tree import Episode


def cluster_episodes(
    episodes: List[Episode],
    random_source: Union[int, np.random.Generator],
    num_clusters: int,
) -> Dict[int, List[Episode]]:
    """Returns a map with <num_clusters> entries from cluster ids to lists of the episodes
    assigned to that cluster.

    The episodes are roughly clustered by the cost reduction sequence of states in the episodes.

    More concretely: the are clustered based on the cost reduction sequence, padded with last
    element in the sequence, where each element in sequence is divided by number of steps
    until sequence end (to heuristically make elements at the end of the sequence matter more).
    """
    # All episodes start with same initial node
    assert len(set([episode[0].node for episode in episodes])) == 1
    cost_sequences = [
        [node.exprenv.cost() for node, _action in episode] for episode in episodes
    ]
    seed = utils.seed(random_source)

    # Make cost sequences equal length by padding with the last cost
    max_len = max([len(seq) for seq in episodes])
    data = [seq + seq[-1:] * (max_len - len(seq)) for seq in cost_sequences]

    # make the cost towards the end of the sequence matter more
    init_cost = cost_sequences[0][0]
    data = np.array(
        [
            [(init_cost - c) / float(max_len - i) for i, c in enumerate(episode)]
            for episode in data
        ],
        dtype=np.float64,
    )

    # K-means clustering
    kmeans = KMeans(n_clusters=num_clusters, random_state=seed).fit(data)

    # Sometimes the labels are permuted/swapped (nondeterministically).
    # Restore determinism by relabelling, in order of the first episode within each.
    # This uses the enumeration ordering - later entries overwrite earlier ones with the same key:
    label_mapping = {}
    for label in kmeans.labels_:
        label_mapping.setdefault(label, len(label_mapping))
    labels = [label_mapping[label] for label in kmeans.labels_]

    # Compute the list of episodes for each cluster and the minimum cost reduction within each cluster
    mapping = defaultdict(list)
    for episode, label in zip(episodes, labels):
        mapping[label].append(episode)

    min_reduction = {
        label: min([init_cost - episode[-1].node.exprenv.cost() for episode in cluster])
        for label, cluster in mapping.items()
    }

    num_members = [len(mapping[key]) for key in mapping.keys()]
    min_reduction = [min_reduction[key] for key in min_reduction.keys()]
    cost = utils.round_to_tol(kmeans.inertia_)
    analytics.event(
        "cluster_episodes",
        seed=seed,
        num_episodes=len(data),
        num_clusters=num_clusters,
        num_members=num_members,
        min_cost_reduction=min_reduction,
        cost=cost,
        labels=labels,
    )
    return mapping


def sample_clusters(
    clusters: Iterator[List[Episode]],
    rng: np.random.Generator,
    num_episodes: int = 100,
) -> List[Episode]:
    """
    Sample the same number of episodes from each cluster.
    """
    num_episodes_per_cluster = num_episodes // len(clusters)
    result = []
    for cluster in clusters:
        if len(cluster) > num_episodes_per_cluster:
            # Randomly choose episodes without replacement
            result.extend(utils.permutation(rng, cluster)[:num_episodes_per_cluster])
        else:
            # Each episode is replicated roughly num_episodes_per_cluster // len(episodes) times
            for i, episode in enumerate(cluster):
                n0 = i * num_episodes_per_cluster // len(cluster)
                n1 = (i + 1) * num_episodes_per_cluster // len(cluster)
                result.extend([episode] * (n1 - n0))
    return result
