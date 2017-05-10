#include <stdio.h>
#include <string.h>
#include <curl/curl.h>
#include "filedwld.h"
 
int geturl(char *url,  char *outfname)
{
  CURL *curl;
  CURLcode res;
  FILE *fp;
  // char outfname[] = "www.cs.columbia.edu/~sedwards/classes/2016/4115-fall/ocaml.pdf";
  // char *outfname = url;
  curl = curl_easy_init();
  if(curl) {
    fp = fopen(outfname, "w");
    curl_easy_setopt(curl, CURLOPT_URL, url);
    /* example.com is redirected, so we tell libcurl to follow redirection */ 
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); 
    /* Perform the request, res will get the return code */ 
    res = curl_easy_perform(curl);
    /* Check for errors */ 
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    /* always cleanup */ 
    curl_easy_cleanup(curl);
    fclose(fp);
  }
  return 0;
}

