# LIS 590: Directed Fieldwork at Stevenson Elementary School Conducting Diversity Audit

## Table of Contents

1. [General Information](#general-information)
2. [Atrifact Repository Contents](#artifact-repository-contents)
3. [Data Dictionary](#data-dictionary)
4. [Metadata](#metadata)
5. [Contact](#contact)

## General Information

My name is Connor Franklin Rey. I am a second-year residential student in the University of Washington iSchool's MLIS program. This is the artifact repository for my Fall '23 Directed Field Work with Stevenson Elementary School Library. The focus of this DFW is to conduct a diversity audit of the fairytale section, suggest books for weeding, and -- if time allows -- suggest purchases to fill gaps in the collection.

The [weekly reflections folder](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/weekly_reflections) contains written reflections from each week, as well as the final reflection on the 10-week project, which may help provide context on the work behind this project.

The [artifacts](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/artifacts) folder contains documentation of my work. Including the following:

## Artifact Repository Contents:
| Folder | Filename | Description |
| --- | --- | --- |
| [artifacts](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/artifacts) | [stevenson_elementary_school_library_diversity_audit_report.pdf](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/artifacts/stevenson_elementary_school_library_diversity_audit_report.pdf) | This is the final report that was turned in to the site supervisor. It shares the findings of the audit, as well as recommended priorities for weeding and collection development. This report was [originally posted with interactive visualizations on Medium](https://medium.com/@franklinreyconnor/diversity-audit-of-stevenson-elementary-school-librarys-folktale-collection-b1d77c874ae0), but a PDF copy has been made available here.|
| [artifacts](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/artifacts) | [folktale_weeding_suggestions.csv](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/artifacts/folktale_weeding_suggestions.csv) | This is a list of the folktale collection catalog, ordered from highest priority of weeding to lowest utilizing the criteria specified in the above project report. |
| [artifacts](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/artifacts) | [stevenson_folktale_diversity_audit.R](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/artifacts/stevenson_folktale_diversity_audit.R) | This is the script I used to conduct the computational analysis of the collection data. The computation was always done locally on my computer and some filenames have been changed since I last worked on it, so some tweaking would be needed to recreate my work. I hope the code is well-commented enough to provide proper context. |
| [data](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/artifacts/data) | [folktale_collection_catalog.csv](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/artifacts/data/folktale_collection_catalog.csv) | This is a copy of the bibliographic information of the folktale collection from the library catalog. |
| [data](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/artifacts/data) | [folktale_collection_circulation_report.csv](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/artifacts/data/folktale_collection_circulation_report.csv) | This is a copy of a circulation report of the books in the folktale collection, tallying the total number of checkouts for each book. |
| [data](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/tree/main/artifacts/data) | [folktale_collection_diversity_data.csv](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/artifacts/data/folktale_collection_diversity_data.csv) | This is the dataset I collected to record the diversity within the collection.  My methodology is explained in the project report. |

## Data Dictionary

The following data dictionary explains the different variables in all datasets used in this diversity audit, the specific file(s) any variable is contained within is specified in the "File(s)" column.

| Variable | Variable Type | Description | File(s) |
| ------ | ------ | ------ | ----- | 
| Barcode | integer | The barcode associated with each book. Used as the main identifier to link records across datasets (except for the circulation report dataset, which did not allow for the download of associated barcode information, so that dataset was linked by the Title/Subtitle variable. | folktale_collection_catalog.csv , folktale_collection_diversity_data.csv , folktale_weeding_suggestions.csv |
| Call Number | string | The Dewey decimal call number used within the library | folktale_collection_catalog.csv , folktale_weeding_suggestions.csv |
| Author | string | The primary creator of the work | folktale_collection_catalog.csv , folktale_weeding_suggestions.csv |
| Publication Year | date | The year of publication | folktale_collection_catalog.csv , folktale_weeding_suggestions.csv |
| Subject | string | The first LoC Subject Heading associated with each book | folktale_collection_catalog.csv , folktale_weeding_suggestions.csv |
| Title/Subtitle | string | The title and subtitle of the book | folktale_collection_catalog.csv , folktale_collection_circulation_report.csv , folktale_weeding_suggestions.csv |
| Checkouts - Local Patrons | integer | Total checkouts from student library users for associated book since December 2, 2023 (when data was pulled) | folktale_collection_circulation_report.csv , folktale_weeding_suggestions.csv |
| Checkouts - Visiting Patrons | integer | Total checkouts from visiting library users for associated book since December 2, 2023 (when data was pulled) | Not utilized when analyzing the circulation of collection. | folktale_collection_circulation_report.csv , folktale_weeding_suggestions.csv |
| Timestamp	| date | The time when I recorded the diversity information associated with the book. | folktale_collection_diversity_data.csv , folktale_weeding_suggestions.csv |
| Representation indicators from cover and summary | string | Tags given to indicate instances of diverse representation in the depiction of characters or central themes of the story, appropriated from Cedar Rapids Public Library diversity audit. I want to acknowledge that some of these tags do not accurately or respectfully describe the cultural groups they intend to encompass. I apologize for the harm they perpetuate, and I intend to utilize more respectful and accurate terms in future audits. </br> </br> **Definition of tags:** </br> - Animals and/or mythical creatures : Depictions of animals and or mythical creatures as central figures of story </br> - Race/Ethnicity : Depiction of BIPOC as central figures of story </br> -  Economic Welfare : Representation of working-class people </br> - LGBTQIA+ : Depiction of LGBTQIA+ people </br> - Mental Health : Depiction of neurodiversity </br> - Physical Health : Depiction of people with physical disabilities </br> - Religion : Depiction of religious diversity </br> - multicultural : multicultural story </br> - africa : story originates from an African culture </br> - AAPI : Story originates from an Asian or Pacific Islander culture </br> - latin america : Story originates from a Latin American culture </br> - indigenous : Story originates from Native American culture </br> - arabic : Story originates from Middle East or North African culture </br> - Black : Depicition of Black Americans </br> </br> Any additional tags were gathered from LoC subject headings seen on the publication page of the book. See [project report](https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/artifacts/stevenson_elementary_school_library_diversity_audit_report.pdf) for further explanation of methodology. For future audits, I would recommend recording each tag as a T/F value in its own column for easier computational analysis. | folktale_collection_diversity_data.csv , folktale_weeding_suggestions.csv |
| Representation.indicators.from.cover.and.summary_no_animals | string | Representation information with "Animals and/or mythical creatures" removed | folktale_weeding_suggestions
| intersectionality_score | integer | A count of the number of diverse representation tags associated with a book. Used to gain a general sense of intersectional diversity -- or lack their of -- and determine priorities for weeding. A book with no tags in its "Representation indicators from cover and summary" column receives a score of 0 and is considered high priority for weeding. | folktale_weeding_suggestions.csv |
| intersectionality_score_no_animals | integer | Intersectionality score calculated from Representation.indicators.from.cover.and.summary_no_animals column | folktale_weeding_suggestions.csv |
| Language | string | The language or languages of the book. | folktale_collection_diversity_data.csv , folktale_weeding_suggestions.csv|

## Metadata
Metadata Schema: [Project Open Data](https://resources.data.gov/resources/dcat-us)
| Attribute | Value |
| --- | --- |
| title | LIS 590: Directed Fieldwork at Stevenson Elementary School Conducting Diversity Audit |
| description | This is the artifact repository for my Fall '23 Directed Field Work with Stevenson Elementary School Library. The focus of this DFW is to conduct a diversity audit of the fairytale section and suggest priorities for weeding collection development. |
| keyword | diversity, diversity audit, school library, children's literature, folktales, library data|
| issued | 11/27/2023 |
|modified| 12/08/2023|
|publisher| Connor Franklin Rey |
|contactPoint|Connor Franklin Rey, franklinreyconnor@gmail.com |
|accessLevel | public|
|license|https://opendatacommons.org/licenses/pddl/|
|temporal| 1957-2019|
|describedBy|https://github.com/c-f-rey/lis_590_dfw_diversity_audit/blob/main/README.md|
|language|English|
|theme| Diversity in school library collections |
|accessURL |lis_590_dfw_diversity_audit|
|format|CSV|

## Contact
Connor Franklin Rey  franklinreyconnor@gmail.com
