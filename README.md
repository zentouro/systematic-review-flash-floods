## Systematic Review Software


An R-shiny App to make systematically reviewing easier in the Natural and Social Sciences. 

This app is still very much in development, but feel free to share and use! Let us know if you have any suggestions. 


### How to use
Working on a systematic review? Here's our current workflow. 

#### Step 1

Create your screening data using `Set-up code.R`
While creating this file, take some time to think through what information and questions you'll be asking for this analysis. We recommend constructing a series of Yes/No questions you can ask of every abstract. Here are ours:

```
1. Is the paper relevant to flash flooding or hydrology in general? (If NO - Not Relevant)
2. If Yes, is the paper about more than the underlying hydrology behind flooding (If NO - Hydrology)
3. If Yes the paper will likely be included in our analysis. What kind of paper is it?
    1. Is the paper primarily geophysically focused ()? Or socio-politically/impact focused?  (Select appropriate toggle, it can be both)
    2. Is the paper about an event? (If YES - Event)
4. Does the paper disaggregate by flood type? (if NO - Not Disaggregated)
5. Is the paper extremely relevant to our analysis and questions about the impact and vulnerability associated with flash floods? (If YES - Review Database) 
```

These questions directly inform the columns we created during the screening data set-up. 

#### Step 2

With questions and screening data in hand, using `MainSystReviewCode.R` you can begin the screening process.
Adjust the text in the shiny app to align with your questions and columns. And go to town reviewing papers. 

  + ##### Step 2A - How we keep organized

  + After finishing a screening session, I recommend copying the `screeningData.rData` file into the `03_backup-screening-data` directory. Should I make this happen automatically? Yes. Have I done that yet. No. Maybe once I've finished actually doing the systematic review. 


### Support

Something not working? Have any questions? 

Tweet at me <a href="http://twitter.com/zentouro" target="_blank">`@zentouro`</a>

### Whodunit ✨

This project exists because of these excellent folks:

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="http://zentouro.ldeo.columbia.edu/"><img src="https://avatars0.githubusercontent.com/u/7304202?v=4" width="100px;" alt=""/><br /><sub><b>miriam</b></sub></a><br /><a href="https://github.com/zentouro/systematic-review-flash-floods/commits?author=zentouro" title="Code">💻</a></td>
    <td align="center"><a href="https://github.com/hgreatrex"><img src="https://avatars2.githubusercontent.com/u/5038576?v=4" width="100px;" alt=""/><br /><sub><b>Helen Greatrex</b></sub></a><br /><a href="https://github.com/zentouro/systematic-review-flash-floods/commits?author=hgreatrex" title="Code">💻</a></td>
  </tr>
</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->


This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome! ([emoji key](https://allcontributors.org/docs/en/emoji-key))

Feel free to 🍴 this repo 
or <a href="https://github.com/zentouro/systematic-review-flash-floods.git" targets="_blank"> clone it to your own machine </a> 
and review to your hearts content. 

Pull requests are also welcome. 
For major changes, please open an issue first to discuss what you would like to change.



### Understanding Our File Structure

This directories in this GitHub are organized to follow the timeline of use and creation. 

within `/data` 

> `01_Web-Of-Science` contines the original .bib files pulled from the Web of Science Database. This service only allows downloading in 500 item groupings. 

> `02_Covidence` contains the output from our initial Phase 1 screening using the software tool Covidence. The folder contans csv and .ris files of the abstracts that were deemed Yes or Maybe and the abstracts that were deemed NO. These were used in the creation of the dataframe used in the Phase 2 screening. 

> `03_clean-screening-data` includes two files: `UNSCREENED-OLD-screeningDATA` and `UNSCREENED-screeningData`. The `OLD` file contains the initial screening test file that was used during the development of Step 1, outlined above. The other file contains all abstracts used in the initial Step 1 screening after the process had been determined. 

> `04_backup-screening-data` containes backup copies of the dataframe as we screened. 

> `05_screened-data` holds the data for the finished full abstract screening. One file `BACKUP` is not to be touched and serves as a failsafe if the `ACTIVE` data develops issues during analysis. 

within `/code`

> `01_screening-code` contains all R files used in the intial full screening 

> `02_analysis-code` contains all R files used in data analysis. This folder is currently in development.

within `/exports`

> exports will contain plots created during analysis. it also contains exports from software VOSViewer. 

within `of interest`

> screenshots and papers that are relevant to this review

within `zz-misc`

> will contain misclenaous, testing, or other items we are not yet ready to delete

