import numpy as np
import re
from sklearn.metrics.pairwise import cosine_similarity
from nltk.corpus import stopwords
# 加载删减版模型
import os
import nltk
from gensim.models import KeyedVectors
model_path = "filtered_word2vec_model.kv"  # 替换为删减版模型的路径
model = KeyedVectors.load(model_path)

nltk.data.path.append(os.path.join(os.getcwd(), "nltk_data"))

# 停用词集合
extra_stopwords = {
    "ad", "episode", "podcast", "instagram", "podcasts", "dr", "youtube",
    "subscribe", "twitter", "episodes", "including", "alex", "links", "ll",
    "access", "sponsors", "ve", "don", "spotify", "facebook", "kelly", "john",
    "sean", "joe", "called", "host", "david", "website", "ben", "dive", "tom",
    "mike", "hosted", "channel", "times", "de", "ramsey", "tim", "list", "web",
    "chris", "click", "dan", "amazon", "ryan", "megyn", "follow", "exclusive",
    "shapiro", "tiktok", "guided", "multitude", "intended", "siriusxm", "free",
    "link", "visit", "discord"
}
stop_words = set(stopwords.words('english')).union(extra_stopwords)

# 清洗文本函数
def clean_text(text):
    text = re.sub(r'[^\w\s]', '', text.lower())  # 去标点并小写化
    words = [word for word in text.split() if word not in stop_words]  # 去停用词
    return " ".join(words)

# 文本向量化函数
def get_sentence_vector(sentence, model):
    words = sentence.split()
    word_vectors = [model[word] for word in words if word in model]
    if word_vectors:
        return np.mean(word_vectors, axis=0)
    else:
        return np.zeros(model.vector_size)

# 加载主题向量
science_vector = np.load("science_vector.npy")
finance_vector = np.load("finance_vector.npy")
thrilling_vector = np.load("thrilling_vector.npy")

# 给定的 Box-Cox 参数和 MinMax 范围
boxcox_params = {
    "science": {"lambda": 1.6699, "min": -0.5988, "max": -0.1486},
    "finance": {"lambda": 1.2400, "min": -0.8064, "max": -0.1334},
    "thrilling": {"lambda": 1.9440, "min": -0.5144, "max": -0.1170},
}

def boxcox_transform(value, params):
    transformed = (value ** params["lambda"] - 1) / params["lambda"]
    return transformed

def minmax_normalize(value, params):
    normalized = (value - params["min"]) / (params["max"] - params["min"])
    return normalized

# 相似度计算和归一化函数
def calculate_similarity_scores(new_sample):
    # 清洗文本
    cleaned_sample = clean_text(new_sample)
    #print(f"Cleaned Text: {cleaned_sample}")  # 输出清洗后的文本

    # 转换为向量
    sample_vector = get_sentence_vector(cleaned_sample, model)
    if sample_vector is None or not sample_vector.any():  # 如果向量化失败
        print("Sample vectorization failed.")
        return None  # 返回空值

    try:
        # 计算与主题的相似度
        similarity_science = cosine_similarity(sample_vector.reshape(1, -1), science_vector.reshape(1, -1)).flatten()[0]
        similarity_finance = cosine_similarity(sample_vector.reshape(1, -1), finance_vector.reshape(1, -1)).flatten()[0]
        similarity_thrilling = cosine_similarity(sample_vector.reshape(1, -1), thrilling_vector.reshape(1, -1)).flatten()[0]

        # Box-Cox 转换
        transformed_science = boxcox_transform(similarity_science, boxcox_params["science"])
        transformed_finance = boxcox_transform(similarity_finance, boxcox_params["finance"])
        transformed_thrilling = boxcox_transform(similarity_thrilling, boxcox_params["thrilling"])

        # MinMaxScaler 归一化
        normalized_science = minmax_normalize(transformed_science, boxcox_params["science"])
        normalized_finance = minmax_normalize(transformed_finance, boxcox_params["finance"])
        normalized_thrilling = minmax_normalize(transformed_thrilling, boxcox_params["thrilling"])

        # 返回归一化后的分数，保留四位小数
        return (
            round(normalized_science, 4),
            round(normalized_finance, 4),
            round(normalized_thrilling, 4)
        )

    except Exception as e:
        print(f"Error in similarity calculation: {e}")
        return None