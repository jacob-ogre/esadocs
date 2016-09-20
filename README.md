## Elasticsearch and an app to search ESA documents

There are thousands of administrative documents about the U.S. Endangered Species Act (ESA) available on the internet, thousands that are not yet publicly available, and thousands more produced each year. We have gathered PDFs of what is publicly available and added our copies of documents acquired through other means (e.g., Freedom of Information Act [FOIA] requests) to a base collection. The plain text of each document has been extracted or Optical Character Recognition (OCR) used to identify the text. All of this is loaded into an [Elasticsearch](https://www.elastic.co) database, and a web app developed to facilitate searching all of these documents.

### Document structure

Each document in the elasticsearch database includes the following fields:

- `index`: esadocs

- `type`: type of document, including

    - five\_year\_review
  
    - federal\_register
    
    - recovery\_plan
    
    - section\_7a1
    
    - section\_7a2
    
    - section\_10a1A
    
    - CCA
    
    - CCAA
    
    - HCP
    
    - SHA
    
    - misc
    
- `raw_txt`, the raw text of the document, for index and search

- `txt`, the path to the text file

- `pdf`, the path to the pdf

- `basename`, the base name of the pdf and txt files for joining

- `file_name`, the text (`a` tag) from the ECOS link of the document, or another name to ID the document

- `orig_link`, the original URL (`href`) from ECOS

### Errors?

Were you searching for a document and find an error? That's entirely possible, especially for documents where the text was extracted by OCR from a PDF with low-resolution pages. If you have a correct version - either because you have the original, manually entered the text, or by other means - then please [get in touch](mailto:esa@defenders.org). We plan to offer a more automated version of error correction, e.g., texts in a git repo with the opportunity to fork and submit pull requests, in the future. For now, we will make corrections manually.

### Additions welcome

Do you have or know of ESA-related documents that could be added to our database? Please [get in touch](mailto:esa@defenders.org) to discuss how we can work together to make publicly available as much information as possible.

