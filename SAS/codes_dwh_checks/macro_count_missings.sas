*Count No. of missing for each numeric variable in a data frame;
%macro count_missing_numeric(data=, out=missing_counts);

    /* Step 1: Extract only numeric variable names */
    proc contents data=&data out=_vars_(keep=name type) noprint;
    run;

    data _num_vars_;
        set _vars_;
        if type = 1; /* 1 = numeric */
    run;

    /* Step 2: Build missing count expressions */
    proc sql noprint;
        select catx(' ', 'sum(missing(', name, ')) as', name)
        into :missing_exprs separated by ', '
        from _num_vars_;
    quit;

    /* Step 3: Create the output table with missing counts */
    proc sql;
        create table &out as
        select &missing_exprs
        from &data;
    quit;

    /* Clean up */
    proc datasets library=work nolist;
        delete _vars_ _num_vars_;
    quit;

%mend;


*Count No. of missing for each character variable in a data frame;
%macro count_missing_char(data=, out=missing_counts_char);

    /* Step 1: Extract only character variable names */
    proc contents data=&data out=_vars_(keep=name type) noprint;
    run;

    data _char_vars_;
        set _vars_;
        if type = 2; /* 2 = character */
    run;

    /* Step 2: Build missing count expressions */
    proc sql noprint;
        select catx(' ', 'sum(missing(', name, ')) as', name)
        into :missing_exprs separated by ', '
        from _char_vars_;
    quit;

    /* Step 3: Create the output table with missing counts */
    proc sql;
        create table &out as
        select &missing_exprs
        from &data;
    quit;

    /* Clean up */
    proc datasets library=work nolist;
        delete _vars_ _char_vars_;
    quit;

%mend;

