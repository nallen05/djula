; this file contains the documentation to the tags implemented in
; "tag-definitions.lisp" if you change anything in this file make damned sure it is
; consistant with "tag-definitions.lisp"!
;
; note: see "tag.lisp" for an explenation of how implementing new tags work

(in-package :djula)

(def-tag-documentation :block '(name)
"Define a block that can be overridden by child templates. Read the documentation
about \"Template inheritance\" for more information."
:from-django-p t
:but-different-from-django-because
"{{block.super}} is currently broken")

(def-tag-documentation :comment '()
"Ignore everything between {% comment %} and {% endcomment %}"
:from-django-p t)

(def-tag-documentation :cycle '(&rest list)
"Cycle among the given strings or variables each time this tag is encountered.

Within a loop, cycles among the given strings/variables each time through the loop:

   {% for o in some_list %}
       <tr class=\"{% cycle 'row1' 'row2' rowvar %}\">
           ...
       </tr>
   {% endfor %}

You can use any number of values, separated by spaces. Values enclosed in double quotes
\(\"\) are treated as string literals, while values without quotes are assumed to
refer to context variables."
:from-django-p t
:but-different-from-django-because
"In Djula templates {% cycle %} is only usefull inside a {% for %} loop, the Django
behavior of {% cycle %} outside of a loop has not been replicated. Also, you have to
use double qoutes (\"), you can't use single quotes to identity string literals.

Cycling between variables [not string literals] is currently broken.")

(def-tag-documentation :debug ()
"Output a whole load of debugging information."
:from-django-p t
:but-different-from-django-because
"Most tags in Djula only effect the part of the template _after_ [or \"below\"] the tag.
So if you put a {% debug %} tag at he top of a template and a {% debug %} tag at the
bottom of the template they might display different information, depending on the tags
seen in between them")

(def-tag-documentation :example-table '(template-path)
"the {% example-table %} tag enables the designer to link to a file full of example
data to use when creating templates for a web applicaion. the values in this file
will later be replaced with real data by the programmer when the templates are used
in the running application.

the example table functions as a contracts between the designer and the programmer:
the programmer makes sure that the data coming from the application matches the
examples and the designer makes sure data that looks like the examples is displayed
correctly.

Examle tables must be filled with key/val pairs where the keys are lisp keywords
mapping to variables and the values are lisp objects [strings, numbers, lists,
plists, etc.]

If the example table \"/example.foo.sexp\" contains:

   progress \"Complete\"
   user (name \"Nick\" drinking \"mimosa\")

and there is a tag

   {% example-table \"/example.foo.sexp\" %}

somewhere at the top of the template, then

   {{progress}}

will be seen as

   Complete

and

   {{user.name}} is drinking a {{user.drinking}}

will be seen as 

   Nick is drinking a mimosa

The name of an example table must match one of the regular expressions in the
list *EXAMPLE-TABLE-REGEXEPS*. By default, an example table must look like:

   example.XYZ.sexp
   example.XYZ.lisp
   example.XYZ.cl

see also: {% example %}, {% show-table %}")

(def-tag-documentation :example '(variable-name language/value-plist)
"the {% example %} tag enables the designer to provide example data without
involving an example table [see {% example-table %}]

so, during development, if there is a tag

   {% example user \"Nick\" %}

then subsequent occurences of

   {{user}}

will show up as

   Nick

see also: {% example-table %}")

(def-tag-documentation :set-language '(language-name)
"After this tag is seen, the current template starts using the language named
`LANGUAGE-NAME'.")

(def-tag-documentation :translation-table '(template-path)
"{% translation-table %} tags allow the designer to make pages that display
different text when rendered in different languages.

A {% translation %} tag makes everything below them in the template aware of the
translations inside the file pointed to be `TEMPLATE-PATH'.

the file pointed to by `TEMPLATE-PATH' should be filled with Lisp lists that start
with the name of a variable [as a lisp keyword] and end with a plist of
language/value pairs.

So if the translation table \"inter.dictionary.sexp\" contains:

   (hello english \"Hello\" spanish \"Hola\")
   (good-afternoon english \"Good afternoon\" spanish \"Buenas tardes\")

then the template

   {% translation-table \"/hello.sexp\" $}
   {_hello_}, Jill. {_good-afternoon_}.

will be rendered in English as:

   Hello, Jill. Good afternoon.

and in Spanish as:

   Hola, Jill. Buenas tardes.

Note that translations can have nested tags, filters, comments, etc in them.

see also: {% set-language %}, {% show-language %}, {% translation %}, and
{% show-table %}")

(def-tag-documentation :translation '(variable-name language/value-plist)
"{% translation %} enables the designer to provide translations without involving
an actual translation table [see {% translation-table %}]

So if there is a tag

   {% translation foo english \"The Foo\" spanish \"El Foo\" %}

then

   {{foo}}.

will show up in English as:

   The Foo.

and in Spanish as

   El Foo.

Note: Although there can be nested tags, filters, comments, etc in dictionaries,
there cannot be in the values of variables inside {% dictionary-value %} tags.

see also: {% set-language %}, {% show-language %},  {% translation-table %}")

(def-tag-documentation :else '()
"See {% if %}"
:from-django-p t)

(def-tag-documentation :elseif '(var)
"See {% if %}"
:from-django-p t)

(def-tag-documentation :emit-js ()
"prints <script/> elements for all the {% js %} and {% js-script %} tags seen thus far
in the template, in order.

see also {% js %} and {% js-script %}
")

(def-tag-documentation :endblock '(&optional blockname)
"Ends a block created by a {% block %} tag. Read the documentation about \"Template
inheritance\" for more information."
:from-django-p t)

(def-tag-documentation :endcomment '()
"See {% comment %}"
:from-django-p t)

(def-tag-documentation :endemit-js '()
"See {% emit-js %}")

(def-tag-documentation :endfilter '()
"See {% filter %}"
:from-django-p t)

(def-tag-documentation :endfor '()
"See {% for %}"
:from-django-p t)

(def-tag-documentation :endif '()
"See {% if %}"
:from-django-p t)

(def-tag-documentation :endifchanged '()
"See {% ifchanged %}"
:from-django-p t)

(def-tag-documentation :endifequal ()
"see {% ifequal %}"
:from-django-p t)

(def-tag-documentation :endifnotequal ()
"see {% ifnotequal %}"
:from-django-p t)

(def-tag-documentation :extends '(name)
"Signal that this template extends a parent template. The form

   {% extends \"base.html\" %}

uses the literal value \"base.html\" as the name of the parent template to extend.

See Template inheritance for more information."
:from-django-p t
:but-different-from-django-because
"Literal values must be double-quoted (\"\") [not single-quoted ('')]. You cannot give
{% extends %} a variable, you must give it a hard-coded string. Note: Djula knows about
your template directory, so you can give it an absolute path starting at the base of the
template directory, not just relative paths starting at the current template.")

(def-tag-documentation :filter '(filters)
"Filter the contents of the variable through variable filters.

Filters can also be piped through each other, and they can have arguments — just like
in variable syntax.

Sample usage:

   {% filter force_escape|lower %}
      This text will be HTML-escaped, and will appear in all lowercase.
   {% endfilter %}"
:from-django-p t)

(def-tag-documentation :for '(var in list &optional reversed)
"Loop over each item in an array. For example, to display a list of athletes provided
in athlete_list:

   <ul>
      {% for athlete in athlete_list %}
         <li>{{ athlete.name }}</li>
      {% endfor %}
   </ul>

You can loop over a list in reverse by using {% for obj in list reversed %}.

The for loop sets a number of variables available within the loop:

Variable                Description

forloop.counter	        The current iteration of the loop (1-indexed)
forloop.counter0        The current iteration of the loop (0-indexed)
forloop.revcounter      The number of iterations from the end of the loop (1-indexed)
forloop.revcounter0     The number of iterations from the end of the loop (0-indexed)
forloop.first           True if this is the first time through the loop
forloop.last            True if this is the last time through the loop
forloop.parentloop      For nested loops, this is the loop \"above\" the current one"
:from-django-p t
:but-different-from-django-because
"In the development version of Django you can unpack values in sublists with commas.
You can't do that with Djula yet")


(def-tag-documentation :if '(var)
"The {% if %} tag evaluates a variable, and if that variable is 'true' (i.e. exists, is
not empty, and is not a false boolean value) the contents of the block are output:

   {% if athlete_list %}
       Number of athletes: {{ athlete_list|length }}
   {% else %}
       No athletes.
   {% endif %}

In the above, if `athlete_list' is not empty, the number of athletes will be displayed
by the {{ athlete_list|length }} variable.

As you can see, the if tag can take an optional {% else %} clause that will be
displayed if the test fails.

if tags may use and, or or not to test a number of variables or to negate a given
variable:

   {% if athlete_list and coach_list %}
       Both athletes and coaches are available.
   {% endif %}

   {% if not athlete_list %}
       There are no athletes.
   {% endif %}

   {% if athlete_list or coach_list %}
       There are some athletes or some coaches.
   {% endif %}

   {% if not athlete_list or coach_list %}
       There are no athletes or there are some coaches (OK, so
       writing English translations of boolean logic sounds
       stupid; it's not our fault).
   {% endif %}

   {% if athlete_list and not coach_list %}
       There are some athletes and absolutely no coaches.
   {% endif %}

if tags don’t allow and and or clauses within the same tag, because the order of logic
would be ambiguous. For example, this is invalid:

   {% if athlete_list and coach_list or cheerleader_list %}

If you need to combine and and or to do advanced logic, just use nested if tags. For
example:

   {% if athlete_list %}
      {% if coach_list or cheerleader_list %}
         We have athletes, and either coaches or cheerleaders!
      {% endif %}
   {% endif %}

Multiple uses of the same logical operator are fine, as long as you use the same
operator. For example, this is valid:

   {% if athlete_list or coach_list or parent_list or teacher_list %}"
:from-django-p t)

(def-tag-documentation :ifchanged '(&rest vars)
"Check if a value has changed from the last iteration of a loop.

The 'ifchanged' block tag is used within a loop. It has two possible uses.

Checks its own rendered contents against its previous state and only displays the
content if it has changed. For example, this displays a list of days, only displaying
the month if it changes:

   <h1>Archive for {{ year }}</h1>

   {% for date in days %}
      {% ifchanged %}<h3>{{ date|date:\"F\" }}</h3>{% endifchanged %}
         <a href=\"{{ date|date:\"M/d\"|lower }}/\">{{ date|date:\"j\" }}</a>
   {% endfor %}

If given a variable, check whether that variable has changed. For example, the
following shows the date every time it changes, but only shows the hour if both the
hour and the date has changed:

   {% for date in days %}
      {% ifchanged date.date %} {{ date.date }} {% endifchanged %}
      {% ifchanged date.hour date.date %}
         {{ date.hour }}
      {% endifchanged %}
   {% endfor %}"
:from-django-p t)

(def-tag-documentation :ifequal '(a b)
  "Output the contents of the block if the two arguments equal each other.

Example:

   {% ifequal user.id comment.user_id %}
      ...
   {% endifequal %}

As in the {% if %} tag, an {% else %} clause is optional.

The arguments can be hard-coded strings, so the following is valid:

   {% ifequal user.username \"adrian\" %}
      ...
   {% endifequal %}

It is only possible to compare an argument to template variables or strings. You cannot
check for equality with Lisp objects such as T or NIL. If you need to test if
something is true or false, use the if tag instead."
  :from-django-p t)

(def-tag-documentation :ifnotequal '(a b)
"Just like {% ifequal %}, except it tests that the two arguments are not equal."
  :from-django-p t)

(def-tag-documentation :include '(template-path)
"Loads a template and renders it with the current context. This is a way of
\"including\" other templates within a template. The `TEMPLATE-NAME' can either be a
variable or a hard-coded (quoted) string, in double quotes.

This example includes the contents of the template 'foo/bar.html':

   {% include \"foo/bar.html\" %}

This example includes the contents of the template whose name is contained in the
variable template_name:

   {% include template_name %}

An included template is rendered with the context of the template that’s including it.
This example produces the output \"Hello, John\":

   Context: variable person is set to \"john\".

   Template: {% include \"name_snippet.html\" %}

   The \"name_snippet.html\" template:

       Hello, {{ person }}

See also: {% ssi %}."
:from-django-p t
:but-different-from-django-because
 "If `TEMPLATE-PATH' is a hard-coded string it must use double quotes [not
single-quotes]. Also, `TEMPLATE-PATH' knows about the template directory, so you can
use an absolute path beginning at the base of the template directory not just a
relative path starting at the current template.")

(def-tag-documentation :js '(src)
"Makes subsequent occurences of {% emit-js %} load the external javascript located
at `SRC'

If the tag

   {% js 'http://ajax.googleapis.com/ajax/libs/prototype/1.6.0.2/prototype.js' %}

appears in a template then subsequent occurences of the {% emit-js %} tag will print the
following HTML element:

  <script type='text/javascript' src='http://ajax.googleapis.com/ajax/libs/prototype/1.6.0.2/prototype.js' ></script>

before the HTML of any previous {% js %} or {% js-script %} tags and after any following
{% js %} or {% js-script %} tags.

if you need to add other attributes to the <script/> tag just add them after the first
`SRC' argument.

see also {% js-script %}, {% emit-js %}
")

(def-tag-documentation :js-script ()
"Makes subsequent occurences of {% emit-js %} run the inline javascript occuring between
{% js-script %} and {% endjs-script %}

If the tag

   {% js-script %}
      new Ajax.Autocompleter('foo-input', 'foo-div', 'http://example.com');
   {% endjs-script %}

appears in a template then subsequent occurences of the {% emit-js %} tag will print the
following HTML element:

   <script type='text/javascript'>
      new Ajax.Autocompleter('foo-input', 'foo-div', 'http://example.com');
   </script>

before the HTML of any previous {% js %} or {% js-script %} tags and after any following
{% js %} or {% js-script %} tags.

see also {% js %}, {% emit-js %}
")

(def-tag-documentation :lisp '(sexp)
"reads `SEXP' as Common Lisp code using the \"cl-user\" package, evaluates it, and
prints the result.

So the tag

   {% lisp (+ 4 5) %}

shows up in the browser as

   9

note: {% lisp %} tags can be turned off by setting the variable *EVAL-LISP-TAGS*
to NIL")

(def-tag-documentation :show-language '()
"prints the name of the language currently being used by the template")

(def-tag-documentation :show-table '(template-path)
"Output the contents of a given dictionary into the page, html-escaping it first.")

(def-tag-documentation :ssi '(template-path)
"Output the contents of a given file into the page.

Like a simple \"include\" tag, {% ssi %} includes the contents of another file — which
must be specified using an absolute path — in the current page:

   {% ssi \"/home/html/ljworld.com/includes/right_generic.html\" %}

If the optional \"parsed\" parameter is given, the contents of the included file are
evaluated as template code, within the current context:

   {% ssi \"/home/html/ljworld.com/includes/right_generic.html\" parsed %}

Note that if you use {% ssi %}, a programmer will need to set *ALLOWED-INCLUDE-ROOTS*
for you as a security measure.

See also: {% include %}."
:from-django-p t
:but-different-from-django-because
"{% ssi %}'s path must be double-quoted to work. Also, Django's ALLOWED_INCLUDE_ROOTS
is called *ALOWED-INCLUDE-ROOTS* and lives in the \"djula\" package")

(def-tag-documentation :templatetag '(argument)
"Output one of the syntax characters used to compose template tags.

Since the template system has no concept of \"escaping\", to display one of the bits
used in template tags, you must use the {% templatetag %} tag.

The argument tells which template bit to output:

Argument	Outputs
openblock	{%
closeblock	%}
openvariable	{{
closevariable	}}
openbrace	{
closebrace	}
opencomment	{#
closecomment	#}"
:from-django-p t)