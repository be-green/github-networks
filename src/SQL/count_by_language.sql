select lang.name, count(repo_name)
from `bigquery-public-data.github_repos.languages`, UNNEST(language) as lang
group by lang.name
limit 100
