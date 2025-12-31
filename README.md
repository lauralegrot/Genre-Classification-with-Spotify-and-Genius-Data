# Genre-Classification-with-Spotify-and-Genius-Data 🎶🎸
## Project description 
* **Multiclass song genre classification (pop, rap, rock)** using a high-dimensional dataset combining **audio features from Spotify API** and **lyrics scraped via Genius API**, addressing automated music labeling for streaming platforms.
* 📌 **Key insight**: lyrics are highly informative for pop and rap, whereas rock genre is better explained by audio features such as energy, valence, and instrumentalness.

### Modeling pipeline and results
* **Text mining and feature engineering pipeline** including lyrics cleaning, lemmatization, stemming, TF-IDF construction, and train-test split (2285 songs, 13,554 predictors).
* **Comparison of multiple models**: multinomial Logistic Regression with Lasso/Ridge/Elastic Net, XGBoost, and Neural Networks with Word2Vec embeddings
* **Best predictive performance achieved by XGBoost (Accuracy ≈ 0.78)**, while Lasso Logistic Regression provided strong interpretability by identifying genre-specific lexical patterns: **romantic terms for pop, slang/drug-related terms for rap**.


