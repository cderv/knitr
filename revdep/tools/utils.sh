RSTUDIO_CLOUD_REVDEP_KEY=vlJPOKJxw96IAMTdpnwit4qaAB32HtW41thOh72T
ID=e0ee9238-3d5f-4db6-ace4-8533735257ef

# Status
curl --silent -H 'x-api-key: '$RSTUDIO_CLOUD_REVDEP_KEY https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check/$ID/status | jq


res=$(curl -H 'x-api-key: '$RSTUDIO_CLOUD_REVDEP_KEY https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check/$ID)

echo $res | jq '. | with_entries(select(.key | endswith("stamp")))'

curl -H 'x-api-key: '$RSTUDIO_CLOUD_REVDEP_KEY https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check/$ID/packages | jq '.revdep_packages | length'

curl --silent -H 'x-api-key: '$RSTUDIO_CLOUD_REVDEP_KEY https://xgyefaepu5.execute-api.us-east-1.amazonaws.com/staging/check/$ID/status/FAILED | jq
