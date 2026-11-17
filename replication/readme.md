## File Structure
All code is under `code`, all data files are stored under `data`, all produced figures and tables are stored under `results`.

## Reproducing these results
All necessary data is included in this github repo. However, if `replication_data.rds` is lost, it can be reproduced by running `rscript code/descriptive_reproduction.R`. This script reproduces the descriptive results (Appendix 2 in the original paper) as best able, collecting from the multiple data sources. While some data can be scraped directly (e.g. the WDI data), anyone using this code without the data included in this github repo would need to download the Human Rights Protection Scores, TOSCO, and UCDP Prior Conflict Databases.

To redo the first half of the analysis (inc. Figure 2, Appendix 4, and residual plots), run `rscript code/one_OLS_regression.R`.

To redo the second half of the analysis (Figure 4), run `rscript code/two_matching_did.R`.

## LLM Statement
For transparency, I used Claude Opus 5.5 ("Claude") for much of this project; specifically, Claude was used for drafting code, debugging code, help with finding API endpoints, and the occasional sanity check. This was within the usage policy (as I confirmed verbally with you), a useful helper for taking on a group project as an individual, and (largely) a test to see if Claude was an effective tool for producing a project of this size. 

Much to my dismay, Claude was imperfect. Even with reasoning set to 'Extra' it repeatedly made mistakes interpreting the text of the paper, making erroneous operationalization assumptions, etc. This was far from a "run it and forget it" session; over multiple days I had to remind Claude of its errors, give it specifically-crafted prompts, etc.

Anyway: yes Claude was used to generate code, no it wasn't used to generate slides, yes everything it produced was hand-verified.

## Citations
Please let me know if this list is incomplete!

Reproducing: https://github.com/SeraphineM/auth_coll_replication

Garbe, L., Maerz, S. F., & Freyburg, T. (2026). Authoritarian collaboration and repression in the digital age: balancing foreign direct investment and control in internet infrastructure. Democratization, 33(1), 85–108. https://doi-org.proxy.library.upenn.edu/10.1080/13510347.2024.2442377

Freyburg, Tina, Lisa Garbe, Seraphine F. Maerz, and Véronique Wavre. Telecommunications Ownership and Control (TOSCO, V2.0). A dataset on ownership of Internet infrastructure in Africa and Asia, 2000-2022. Dataset and Codebook: https://tosco.shinyapps.io/data/.