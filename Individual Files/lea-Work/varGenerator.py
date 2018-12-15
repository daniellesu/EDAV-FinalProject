import pandas as pd

top_10 = pd.read_csv("./top_10v2.csv", index_col = False)

top_10 = top_10.iloc[:, 1:]

dict_str = ''

for i in range(top_10.shape[0]):
	top_10.iloc[i, 0] = top_10.iloc[i, 0].lower()
	top_10.iloc[i, 0] = top_10.iloc[i, 0].replace(" ", "")
	top_10.iloc[i, 0] = top_10.iloc[i, 0].replace("(", "")
	top_10.iloc[i, 0] = top_10.iloc[i, 0].replace(")", "")
	top_10.iloc[i, 0] = top_10.iloc[i, 0].replace(".", "")
	top_10.iloc[i, 0] = top_10.iloc[i, 0].replace("-", "")

dict_str = ''


for i in range(top_10.shape[0]):
	for j in range(top_10.shape[1] ):
		if j == 0:
			dict_str += 'var ' + top_10.iloc[i, j] + " = [{key: 'Danceability', value: "
		elif j == 1:
			pass
		elif j == 2:
			dict_str += str(top_10.iloc[i, j]) + "}, {key: 'Energy', value: "
		elif j == 3:
			dict_str += str(top_10.iloc[i, j]) + "}, {key: 'Loudness', value: "
		elif j == 4:
			dict_str += str(top_10.iloc[i, j]) + "}, {key: 'Valence', value: "
		elif j == 5:
			dict_str += str(top_10.iloc[i, j]) + "}, {key: 'Tempo', value: "
		elif j == 6:
			dict_str += str(top_10.iloc[i, j]) + "}, {key: 'Acousticness', value: "
		elif j == 7:
			dict_str += str(top_10.iloc[i, j]) + "}, {key: 'Speechiness', value: "
		elif j == 8:
			dict_str += str(top_10.iloc[i, j]) + "}, {key: 'Track Popularity', value: "
		elif j == 9:
			dict_str += str(top_10.iloc[i, j]) + "}];"
	print dict_str
	dict_str = ''	
	print ""
