import os
from glob import glob

list_dir = ['bioenergy', 'climate', 'emissions', 'fertilizers', 'gdp', 'inflation', 'pesticides', 
            'm1-monthly', 'm1-quarterly', 'm1-yearly', 'm3-monthly', 'm3-other', 'm3-quarterly', 'm3-yearly',
            'm4-daily', 'm4-hourly', 'm4-monthly', 'm4-quarterly', 'm4-weekly', 'm4-yearly']
list_names = ['*ts2vec*']

for path in list_dir:
    if os.path.isdir(f'./{path}'):
        for name in list_names:
            search_files = glob(os.path.join(path, '**', name), recursive=True)
            for file in search_files:
                try:
                    #os.remove(file)
                    #print(f'Arquivo removido: {file}')
                except Exception as e:
                    print(f'Erro ao remover {file}: {e}')
    else:
        print(f'O diretório ./{path} não existe.')
