import pandas as pd
import numpy as np

data = {
    'year': [2016, 2017, 2018],
    'GDP rate': [2.8, 3.1, 3.0],
    'GDP': ['1.637M', '1.73M', '1.83M']
}

df = pd.DataFrame(data)

lista = [1 , 2 , 3 , 4]
npdata = np.array(lista)
print(npdata.shape)