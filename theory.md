# Theory behind Pijul

Darcs' patch theory is centered around patches, with two primitive
operations, *commutation* and *inversion*. Compared to this, pijul's
theory revolves around files (i.e., states of the working directory at a
given point in time) as well as patches, with a *merge* operation
between patches. In contrast with git, this merge operation is
well-defined and has all expected properties: in technical terms, it is
a *pushout* in a category where files are the objects, and patches are
the arrows. As a consequence of this, to ensure all diverging pairs of
patches have a merge, the set of files is extended to contain normal
files as well as *files in a conflicted state*. These conflicted files
are *rendered* into the working directory by pijul as files with
conflict markings.

Thanks to this categorical construction, the Pijul version of most
algorithms used in darcs is conceptually simple and efficient.

# Background: other solutions

## The file lock model

In ancient times, and also in cloud-based solutions such as Dropbox, authors who want to collaborate use the *file lock model*, in which an author declares that a file should be his/hers for some period of time, and that any change on that file by others during that period will be either prevented by the software, or deleted by the author.

## The first distributed solution: emails

Some authors use email protocols to share modification to a file, but this tends to make merging somewhat difficult. However, this system is truly distributed, with copies of the files replicated on several machines, which makes it much more robust than centralized solutions such as github (even though its underlying system is distributed).

## Three-way merge

Three-way merge (see the [Wikipedia page](https://en.wikipedia.org/wiki/Merge_(version_control)#Recursive_three-way_merge)) is the grandfather of merge algorithms.

The basic idea is, when Alice and Bob are working on the same file U, and produce different versions V and W of U, three-way merge (Unix command `diff3`) merges the changes. It can sometimes merge in a non-intuitive way, which has led some authors to claim it is "inconsistent":
See [here for an example](https://tahoe-lafs.org/~zooko/badmerge/simple.html).

The big drawback of diff3 is that it is a *local* algorithm, working with just three versions, and no global knowledge of the entire repository. It is therefore mostly useful in software where the global state is restricted enough that this merge algorithm makes sense (and actually, cvs, svn, git and mercurial are mostly wrappers around diff3, that enforce these restrictions).

## Patch commutation

Patch commutation was introduced by [darcs](http://darcs.net). It does not produce the awkward cases of other tools, but needs clever hacks to remain efficient as the history of a repository grows.

One case is a "conflict fight", in which darcs can sometimes take exponential time to merge conflicts. More common cases include users not tagging their repositories very often, contrarily to "good practices".

## Pijul's new idea

Pijul is based on [Samuel Mimram and Cinzia di Giusto's theory of patches](http://arxiv.org/abs/1311.3903) for its core principle (basically, that *version control is about pushouts*), plus a number of other algorithmic ideas to represent repositories efficiently.

It is actually a formalization of the intuition behing patch commutation: indeed, in a category where all patches commute, pushouts are simply found by applying patches in any order.

# Filesets and Patches

# Merges and Pushouts

First things first: you don't need any knowledge of category theory in order to understand Pijul. However, knowing about categories might help you understand why Pijul uses the "most canonical" approach to version control. Conversely, understanding the algorithms in Pijul might help you make sense out of explanations such as [this one](http://ncatlab.org/nlab/show/pushout).

When working asynchronously with co-authors, what we really want is not commutation nor three-way merges. It is instead a "minimal common state", such that anything that both co-authors could do after their changes could also be done after that minimal common state.

This is exactly what category theory calls a *pushout*. On the following diagram, starting from a common state $Z$, if Alice creates a patch $f$, and Bob creates a patch $g$, then the *pushout* of $f$ and $g$ is a state of the repository $P$, such that anything they can do later separately (called $j_1$ and $j_2$ on this diagram) can also be done from $P$.

![pushout diagram](/alice-bob-pushout.png)

<!--

Image source:

~~~ {.tex}
\documentclass{article}
\usepackage{amsmath,amssymb,nopageno}
\usepackage[active,pdftex,tightpage]{preview}
\PreviewEnvironment[{[]}]{equation*}
\usepackage[all]{xy}
\begin{document}
 \begin{equation*}
  \xymatrix@C+2em@R+2em{
   Q \ar@/_10pt/@{<-}[ddr]_{j_1} \ar@/^10pt/@{<-}[drr]^{j_2}
     \ar@{<--}[dr]|*+<3pt,3pt>{\scriptstyle u} & & \\
   & P \ar@{<-}[d]_(0.4){i_1} \ar@{<-}[r]^(0.4){i_2} & Y \ar@{<-}[d]^{g}\\
   & X \ar@{<-}[r]_{f} & Z
  }
 \end{equation*}
\end{document}
~~~

-->

A major problem is, not all categories are complete for pushouts. In other words, sometimes there is no minimal common state that is consistent with $f$ and $g$, which means that $f$ and $g$ *conflict*.

However, a construction from category theory, called the *free co-completion* of a category, allows to add all the pushouts to an initial category, provided that the states themselves live in a "lower order" category (in Pijul, the category of files).

## Concretely, in Pijul

Surprisingly enough, this allowed Pijul to solve a long-standing complexity problem in darcs.
In Pijul, files are represented by *graphs of lines*. Note that this is relatively less general than patch commutation, since the data structure needs to be re-calculated when we change the atomic patch operations, whereas new atomic operations can in principle be added to patch commutation-based systems, as long as the commutation tests are defined for any pair of patches.


# Conflicted Files and Rendering

# Anatomy of a Repository

This is after adding

    blabla
    blublu
    blibli

Then deleting line "blublu" and reintroducing it.

![deleting blublu](/deleting-blublu.png)

<!--

Image source:
~~~ {.dot}
digraph{
	size="8,5"
n_000000000000000000000000000000000000000000000000[label="0000000000:"];
n_000000000000000000000000000000000000000000000000->n_eaeae070fee7812184842fea0de40804cc54a64800000000 [color=blue,label="2"];
n_bcab187dc68651a006d56afb64a6631c4268ebd700000000[label="bcab180000:blublu\n"];
n_bcab187dc68651a006d56afb64a6631c4268ebd700000000->n_eaeae070fee7812184842fea0de40804cc54a64804000000 [color=red,label="0"];
n_bcab187dc68651a006d56afb64a6631c4268ebd700000000->n_eaeae070fee7812184842fea0de40804cc54a64802000000 [color=green,label="4"];
n_eaeae070fee7812184842fea0de40804cc54a64800000000[label="eaeae00000:\001\164a"];
n_eaeae070fee7812184842fea0de40804cc54a64800000000->n_eaeae070fee7812184842fea0de40804cc54a64801000000 [color=blue,label="2"];
n_eaeae070fee7812184842fea0de40804cc54a64800000000->n_000000000000000000000000000000000000000000000000 [color=black,label="6"];
n_eaeae070fee7812184842fea0de40804cc54a64801000000[label="eaeae00100:"];
n_eaeae070fee7812184842fea0de40804cc54a64801000000->n_eaeae070fee7812184842fea0de40804cc54a64802000000 [color=red,label="0"];
n_eaeae070fee7812184842fea0de40804cc54a64801000000->n_eaeae070fee7812184842fea0de40804cc54a64800000000 [color=black,label="6"];
n_eaeae070fee7812184842fea0de40804cc54a64802000000[label="eaeae00200:blabla\n"];
n_eaeae070fee7812184842fea0de40804cc54a64802000000->n_bcab187dc68651a006d56afb64a6631c4268ebd700000000 [color=red,label="0"];
n_eaeae070fee7812184842fea0de40804cc54a64802000000->n_eaeae070fee7812184842fea0de40804cc54a64804000000 [color=red,style=dotted,label="1"];
n_eaeae070fee7812184842fea0de40804cc54a64802000000->n_eaeae070fee7812184842fea0de40804cc54a64801000000 [color=green,label="4"];
n_eaeae070fee7812184842fea0de40804cc54a64802000000->n_eaeae070fee7812184842fea0de40804cc54a64803000000 [color=red,style=dashed,label="8"];
n_eaeae070fee7812184842fea0de40804cc54a64803000000[label="eaeae00300:blublu\n"];
n_eaeae070fee7812184842fea0de40804cc54a64803000000->n_eaeae070fee7812184842fea0de40804cc54a64804000000 [color=red,label="0"];
n_eaeae070fee7812184842fea0de40804cc54a64803000000->n_eaeae070fee7812184842fea0de40804cc54a64802000000 [color=green,style=dashed,label="12"];
n_eaeae070fee7812184842fea0de40804cc54a64804000000[label="eaeae00400:blibli\n"];
n_eaeae070fee7812184842fea0de40804cc54a64804000000->n_bcab187dc68651a006d56afb64a6631c4268ebd700000000 [color=green,label="4"];
n_eaeae070fee7812184842fea0de40804cc54a64804000000->n_eaeae070fee7812184842fea0de40804cc54a64803000000 [color=green,label="4"];
n_eaeae070fee7812184842fea0de40804cc54a64804000000->n_eaeae070fee7812184842fea0de40804cc54a64802000000 [color=green,style=dotted,label="5"];
}
~~~

-->

In this picture, 0000000 is the root of the repo
The next node is a file name + permissions
Next is the root of that file

# Advanced Topics

## Line identifiers
