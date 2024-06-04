
[![](https://github.com/flintforge/ob-sql-session/actions/workflows/CI.yml/badge.svg)](https://github.com/flintforge/ob-sql-session/actions)

```{=org}
#+date : [2024-05-29 Wed]
```
```{=org}
#+License: GPL3
```
<https://github.com/flintforge/ob-sql-session>

## Orb babel functions for SQL, with session

`Org/ob-sql.el`{.verbatim} does not provide a session mode because
source blocks are passed as an input file along the connexion arguments
without any terminal (see for instance man:psql, option -c).

`ob-sql-mode.el`{.verbatim} was proposed as an alternative. It relies on
`sql-session`{.verbatim} to open a client connection, then performs a
simple `sql-redirect`{.verbatim} as execution of the sql source block,
before cleaning the prompt.

But more intersting is this comment in comint:
<file:/usr/local/share/emacs/29.3/lisp/comint.el.gz>::3570

Which brings several remarks:

-   Session mode is only required when keeping a state. In my case
    that\'s a `search_path`{.verbatim} variable.

-   `Sql-redirect`{.verbatim} can perfectly handle many batches of
    commands at once, but it relies on
    `accept-process-output`{.verbatim} which is not the best way to
    handle redirections through comint, and sometimes get clunky when
    bursts of outputs occurs.

-   As long as the output is not a stream, relying on the detection of
    the prompt should not be necessary since comint records where last
    output began.

-   Misdirected output happens when using
    `org-babel-execute:sql-mode`{.verbatim} with several commands or
    longer execution times. The problem comes from properly handling
    `accept-process-output`{.verbatim} and termination.

-   Finally what happen if we rely only on the prompt to detect a
    command termination but for batches of commands with buffered output
    and a prompt showing up on each command? Then there\'s no way to
    detect when a batch finishes, except by giving the job enough time
    to complete, or adding a special command in the end.

We have two situations related to the client:

1.  It returns a message from a command. A regular situation we are
    looing for.
2.  It is silent on the output, and we don\'t want to echo every input,
    example: a `drop`{.verbatim} on sqlite, a `\set`{.verbatim} or quiet
    mode on pgcli.

We can conclude there are two solutions to run a SQL batch:

1.  Split the batch, run commands one by one, Identify silent commands
    (starting with `\`{.verbatim} for pgcli, no semi-column at the
    end...), keep the execution on hold while the next command did not
    output, and add short frames for commands to complete buffered
    output.

2.  add a termination command to the batch: an echo command for
    instance, that stays in the client, and not given to the db. A bit
    faster than 1).

The following is reported to work on emacs 27 to 30, org-mode 9.6 and
9.7

``` elisp
(load-file "./ob-sql-session.el")
```

``` elisp
;; (use-package ob-sql-session)
```

``` elisp
(defun do-org-confirm-babel-evaluations (lang body)
  (not
   (or
    (string= lang "elisp")
    (string= lang "sqlite")
    (string= lang "sql-session"))))
(setq org-confirm-babel-evaluate 'do-org-confirm-babel-evaluations)
```

do-org-confirm-babel-evaluations do-org-confirm-babel-evaluations

`sql-comint-sqlite`{.verbatim} in `sql.el`{.verbatim} needs to accept
nil database in order to run sqlite in memory (`ob-sqlite`{.verbatim}
has ++no+ session support ~~either and requires a database~~

-   commit 68aa43885 merged in org 9.7.2: ob-sqlite: Use a transient
    in-memory database by default).

``` {.sql-session engine="sqlite" results="table" database="test.db"}
--.headers on
  create table test(x,y);
  insert into test values ("sqlite",sqlite_version());
  insert into test values (date(),time());
  select * from test;
```

``` {.sql-session engine="sqlite" results="table" database="test.db"}
.headers on
--create table test(x,y);
delete from test;
insert into test values ("sqlite",sqlite_version());
insert into test values (date(),time());
select * from test;
```

``` {.sql-session engine="sqlite" results="table" database="test.db" session="A"}
--delete from test;
insert into test values ('sqlite','3.40');
insert into test values (1,2);
select * from test;
```

``` {.sql-session engine="sqlite"}
--drop table test;
create table test(one text, two int);
select format("sqlite %s",sqlite_version()), date(), time();
```

``` {.sql-session engine="sqlite" database="test.db"}
create table test(a, b);
drop table test;
```

``` {.sql-session engine="sqlite" database="test.db" results="output"}
drop table test;
create table test(one varchar(10), two smallint);
insert into test values('hello', 1);
insert into test values('world', 2);
select * from test;

```

## In order to run sqlite in memory

`sql-database`{.verbatim} can be *nil* and no option given

``` elisp
(defun sql-comint-sqlite (product &optional options buf-name)
  "Create comint buffer and connect to SQLite."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params
         (append options
                 (if (and sql-database ;; allows connection to in-memory database.
                          (not (string-empty-p sql-database)))
                     `(,(expand-file-name sql-database))))))
    (sql-comint product params buf-name)))
```

``` patch
modified   lisp/progmodes/sql.el
@@ -5061,14 +5061,15 @@ sql-sqlite
   (interactive "P")
   (sql-product-interactive 'sqlite buffer))

-(defun sql-comint-sqlite (product options &optional buf-name)
+(defun sql-comint-sqlite (product &optional options buf-name)
   "Create comint buffer and connect to SQLite."
   ;; Put all parameters to the program (if defined) in a list and call
   ;; make-comint.
   (let ((params
          (append options
-                 (if (not (string= "" sql-database))
-                     `(,(expand-file-name sql-database))))))
+                 (if (and sql-database
+                         (not (string= "" sql-database)))
+                         `(,(expand-file-name sql-database))))))
     (sql-comint product params buf-name)))

```

``` {.sql-session engine="sqlite"}
create table test(an int, two char);
SELECT *
  FROM sqlite_schema;
select format("sqlite %s",sqlite_version()), date(), time();

```

create table test(an int, two char); Same session

``` {.sql-session engine="sqlite" session="A"}
create table test(an int, two char);
```

``` {.sql-session engine="sqlite" session="A"}
select format("sqlite %s",sqlite_version()), date(), time();
```

## Test on postgres {#test-on-postgres header-args="sql-session :engine postgres :database test :results table"}

``` {.sql-session dbhost="\"\"" results="output"}
select inet_client_addr(); -- no host=socket
select localtime(0);
select current_date, 'hello world';

```

``` example
21:40:20
2024-06-03|all your nuts are belong to us
```

Session starts

``` {.sql-session session="A"}
select inet_client_addr();
select localtime(0);
select current_date, current_time;

```

Error handling

``` {.sql-session session="A"}
select current_time, 1;
select err;
select 'ok';
```

``` sql-session
\echo :var
```

## Formatting results

We are not relying on the prompt here. They may even be set to empty
string.

``` elisp
(sql-set-product-feature 'postgres :prompt-regexp "SQL> ")
(sql-set-product-feature 'postgres :prompt-cont-regexp "")
(setq sql-postgres-options (list
                            "--set=ON_ERROR_STOP=1"
                            (concat "--set=PROMPT1="
                                    (sql-get-product-feature 'postgres :prompt-regexp ))
                            (concat "--set=PROMPT2="
                                    (sql-get-product-feature 'postgres :prompt-cont-regexp ))
                            "-q"
                            "-P" "pager=off"
                            "-P" "footer=off" "-A"
                            "--tuples-only"
                            ))

```

## Variables

``` {#test-sql-session .sql-session engine="sqlite" var="x=\"3.0\""}
select 1/$x;
```

```{=org}
#+RESULTS: test-sql-session
```
``` example
0.333333333333333
```

## Test against large output

``` {.sql-session engine="postgres" database="test"}
--  drop sequence serial;
  Create sequence serial start 1;
  select nextval('serial'),array(select generate_series(0, 200)) from generate_series(0, 250);
```

\[X\] pass

## [TODO]{.todo .TODO} \> {#section}

-   [ ] Provide password
    [with-environment-variables](/usr/share/emacs/28.2/lisp/env.el.gz::defmacro with-environment-variables)
-   [ ] properties relative to the session (clean-output-regex)
-   [ ] merge into ob-sql?
