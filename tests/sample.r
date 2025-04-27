require(rreport)
download.file('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/Rreport/ssafety.rda', 'ssafety.rda')
Load(ssafety)
ssafety <- upData(ssafety, rdate=as.Date(rdate, tz=''),
 labels=c(rdate='Randomization Date'))

## Save last modification date/time for source data files in
## LaTeX variables datadate and primarydatadate in file params.tex
cat('\\def\\datadate{',   format(file.info('ssafety.rda')$mtime),'}\n',
    '\\def\\primarydatadate{',format(file.info('ssafety.rda')$mtime),'}\n',
    sep='', file='gentex/params.tex')

## List of lab variables that are missing too much to be used
omit  <- Cs(amylase,aty.lymph,glucose.fasting,neutrophil.bands)

## Make a list that separates variables into major categories
vars <- list(baseline=Cs(age,sex,race,height,weight,bmi,smoking,pack.yrs),
             ae  =Cs(headache, ab.pain, nausea, dyspepsia, diarrhea,
                     upper.resp.infect, coad),
             ekg =setdiff(names(ssafety)[c(49:53,55:56)],'atrial.rate'),
             chem=setdiff(names(ssafety)[16:48],
               c(omit, Cs(lymphocytes.abs,atrial.rate,monocytes.abs,
                          neutrophils.seg,eosinophils.abs,basophils.abs)))) 


gtype <- c('ps','pdf','interactive')[2]

require(lattice)

week <- ssafety$week
weeks <- sort(unique(week))

base <- subset(ssafety, week==0)

## Make key for different line styles for inclusion in figure captions
makeTreatKey(levels(base$trx), append=TRUE)  ## adds to params.tex

accrualReport(Minor=base$site, MinorLabel='site',
              EntryDate1=as.chron(base$rdate),
              EntryDate1cap='randomized subjects',
              dateRange=c('1990-01-01','1994-12-31'),
              targetDate='1994-12-31', targetN=300, hdotchart=4)

completenessReport(base, vars$baseline,  'baseline',
                   append=FALSE)
completenessReport(ssafety, vars$ae,     'ae',  week,
                   longPanel='adverse events')
completenessReport(ssafety, vars$ekg,    'ekg',
                   week, weeks[weeks!=1], longPanel='EKG')
completenessReport(ssafety, vars$chem,   'chem',
                   week, weeks[weeks %nin% c(1,16,20)],
                   longPanel='clinical chemistry')

complianceReport(ssafety$comply, ssafety$trx, ssafety$week,
                 weeks[weeks > 1])

baselineReport(base, vars$baseline, treat='trx', cdf=TRUE, long=FALSE)

repVarclus(ssafety[unlist(vars)], week, c(0,8))

dropoutReport(base$d.dropout, base$dropout, base$trx, time.inc=14)

aeReport(ssafety,  vars$ae, 'trx', 'week', weeks, 'id',
         times.tables=c(4,12), ylim=c(0,.15), forceBinary=TRUE)


labReport(ssafety, vars$ekg, 'ekg', 'trx', 'id', 'week', c(0,2,4,8),
          longPanel='EKG', cdf=c('corr.qt','pr'), clearPlots=TRUE)


labReport(ssafety, vars$chem, 'chem', 'trx', 'id', 'week', c(0,2,4,8,12),
          longPanel='Clinical Chemistry',
          cdf=c('rbc','asat','creatinine'))

             
if(gtype=='ps') dirps2pdf()  # if want to convert all new ps to pdf files
