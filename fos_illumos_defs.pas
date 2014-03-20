unit fos_illumos_defs;

{
  This gathers headers needed for other interfaces:
  avl.h }

{
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
  }
{
 * Copyright 2009 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 * Copyright 2014 FirmOS Business Solutions GmbH
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,ctypes,unix,baseunix,unixtype;

type
  {FOS Alias}
  boolean_t   = (B_FALSE,B_TRUE);
  int16_t     = cInt16;
  int32_t     = cint32;
  Hrtime_t    = cuint64;
  Uchar_t     = cuchar;
  Uint64_t    = cuint64;
  int64_t     = cint64;
  int8_t      = cint8;
  uint8_t     = cuint8;
  ulong_t     = culong;
  uint16_t    = cuint16;
  UInt32_t    = cuint32;
  uint_t      = cuint;
  cuintptr_t  = PtrUInt;


  Pboolean_t       = ^boolean_t;
  PPboolean_t      = ^Pboolean_t;
  Pdouble          = ^double;
  Phrtime_t        = ^hrtime_t;
  Pint16_t         = ^int16_t;
  Pint32_t         = ^int32_t;
  Pint64_t         = ^int64_t;
  PPint64_t        = ^Pint64_t;
  Pint8_t          = ^int8_t;
  PPcchar          = ^Pcchar;
  PPPcchar         = ^PPcchar;
  PPUchar_t        = ^Puchar_t;
  PPint8_t         = ^Pint8_t;

  Psize_t          = ^csize_t;
  Puchar_t         = ^uchar_t;
  Puint16_t        = ^uint16_t;
  PPuint16_t       = ^Puint16_t;
  Puint32_t        = ^uint32_t;
  PPuint32_t       = ^Puint32_t;
  Puint64_t        = ^uint64_t;
  PPuint64_t       = ^Puint64_t;
  Puint8_t         = ^uint8_t;
  PPuint8_t        = ^Puint8_t;
  Puint_t          = ^uint_t;
  PPint16_t        = ^Pint16_t;
  PPint32_t        = ^Pint32_t;


  {
 * Direction constants used for avl_nearest().
  }

const
  AVL_BEFORE = 0;
  AVL_AFTER = 1;
  external_avl_library = 'avl'; // need to link in illumos(omnios) ?

Type
  Pavl_index_t  = ^avl_index_t;
  Pavl_tree_t  = ^avl_tree_t;

{
 * This is a generic implemenatation of AVL trees for use in the Solaris kernel.
 * The interfaces provide an efficient way of implementing an ordered set of
 * data structures.
 *
 * AVL trees provide an alternative to using an ordered linked list. Using AVL
 * trees will usually be faster, however they requires more storage. An ordered
 * linked list in general requires 2 pointers in each data structure. The
 * AVL tree implementation uses 3 pointers. The following chart gives the
 * approximate performance of operations with the different approaches:
 *
 *	Operation	 Link List	AVL tree
 *	---------	 --------	--------
 *	lookup		   O(n)		O(log(n))
 *
 *	insert 1 node	 constant	constant
 *
 *	delete 1 node	 constant	between constant and O(log(n))
 *
 *	delete all nodes   O(n)		O(n)
 *
 *	visit the next
 *	or prev node	 constant	between constant and O(log(n))
 *
 *
 * The data structure nodes are anchored at an "avl_tree_t" (the equivalent
 * of a list header) and the individual nodes will have a field of
 * type "avl_node_t" (corresponding to list pointers).
 *
 * The type "avl_index_t" is used to indicate a position in the list for
 * certain calls.
 *
 * The usage scenario is generally:
 *
 * 1. Create the list/tree with: avl_create()
 *
 * followed by any mixture of:
 *
 * 2a. Insert nodes with: avl_add(), or avl_find() and avl_insert()
 *
 * 2b. Visited elements with:
 *	 avl_first() - returns the lowest valued node
 *	 avl_last() - returns the highest valued node
 *	 AVL_NEXT() - given a node go to next higher one
 *	 AVL_PREV() - given a node go to previous lower one
 *
 * 2c.  Find the node with the closest value either less than or greater
 *	than a given value with avl_nearest().
 *
 * 2d. Remove individual nodes from the list/tree with avl_remove().
 *
 * and finally when the list is being destroyed
 *
 * 3. Use avl_destroy_nodes() to quickly process/free up any remaining nodes.
 *    Note that once you use avl_destroy_nodes(), you can no longer
 *    use any routine except avl_destroy_nodes() and avl_destoy().
 *
 * 4. Use avl_destroy() to destroy the AVL tree itself.
 *
 * Any locking for multiple thread access is up to the user to provide, just
 * as is needed for any linked list implementation.
  }
{
 * Type used for the root of the AVL tree.
  }
  //avl_tree =
  avl_tree_t = record end;
{
 * The data nodes in the AVL tree must have a field of this type.
  }
  //avl_node =
  avl_node_t = record end;
{
 * An opaque type used to locate a position in the tree where a node
 * would be inserted.
  }

  avl_index_t = cuintptr_t;
{
 * Prototypes
 *
 * Where not otherwise mentioned, "void *" arguments are a pointer to the
 * user data structure which must contain a field of type avl_node_t.
 *
 * Also assume the user data structures looks like:
 *	stuct my_type
 *		...
 *		avl_node_t	my_link;
 *		...
 *	;
  }
{
 * Initialize an AVL tree. Arguments are:
 *
 * tree   - the tree to be initialized
 * compar - function to compare two nodes, it must return exactly: -1, 0, or +1
 *          -1 for <, 0 for ==, and +1 for >
 * size   - the value of sizeof(struct my_type)
 * offset - the value of OFFSETOF(struct my_type, my_link)
  }
(* Const before type ignored *)
(* Const before type ignored *)

  TAVLCompar = function (_para1:pointer; _para2:pointer):cint ;cdecl;

procedure avl_create(tree:Pavl_tree_t; compar : TAVLCompar; size:size_t; offset:size_t);cdecl;external external_avl_library name 'avl_create';
{
 * Find a node with a matching value in the tree. Returns the matching node
 * found. If not found, it returns NULL and then if "where" is not NULL it sets
 * "where" for use with avl_insert() or avl_nearest().
 *
 * node   - node that has the value being looked for
 * where  - position for use with avl_nearest() or avl_insert(), may be NULL
  }
(* Const before type ignored *)
function avl_find(tree:Pavl_tree_t; node:pointer; where:Pavl_index_t):pointer;cdecl;external external_avl_library name 'avl_find';
{
 * Insert a node into the tree.
 *
 * node   - the node to insert
 * where  - position as returned from avl_find()
  }
procedure avl_insert(tree:Pavl_tree_t; node:pointer; where:avl_index_t);cdecl;external external_avl_library name 'avl_insert';
{
 * Insert "new_data" in "tree" in the given "direction" either after
 * or before the data "here".
 *
 * This might be usefull for avl clients caching recently accessed
 * data to avoid doing avl_find() again for insertion.
 *
 * new_data	- new data to insert
 * here		- existing node in "tree"
 * direction	- either AVL_AFTER or AVL_BEFORE the data "here".
  }
procedure avl_insert_here(tree:Pavl_tree_t; new_data:pointer; here:pointer; direction:cint);cdecl;external external_avl_library name 'avl_insert_here';
{
 * Return the first or last valued node in the tree. Will return NULL
 * if the tree is empty.
 *
  }
function avl_first(tree:Pavl_tree_t):pointer;cdecl;external external_avl_library name 'avl_first';
function avl_last(tree:Pavl_tree_t):pointer;cdecl;external external_avl_library name 'avl_last';
{
 * Return the next or previous valued node in the tree.
 * AVL_NEXT() will return NULL if at the last node.
 * AVL_PREV() will return NULL if at the first node.
 *
 * node   - the node from which the next or previous node is found
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function AVL_NEXT(tree,node : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function AVL_PREV(tree,node : longint) : longint;

{
 * Find the node with the nearest value either greater or less than
 * the value from a previous avl_find(). Returns the node or NULL if
 * there isn't a matching one.
 *
 * where     - position as returned from avl_find()
 * direction - either AVL_BEFORE or AVL_AFTER
 *
 * EXAMPLE get the greatest node that is less than a given value:
 *
 *	avl_tree_t *tree;
 *	struct my_data look_for_value = ....;
 *	struct my_data *node;
 *	struct my_data *less;
 *	avl_index_t where;
 *
 *	node = avl_find(tree, &look_for_value, &where);
 *	if (node != NULL)
 *		less = AVL_PREV(tree, node);
 *	else
 *		less = avl_nearest(tree, where, AVL_BEFORE);
  }
function avl_nearest(tree:Pavl_tree_t; where:avl_index_t; direction:cint):pointer;cdecl;external external_avl_library name 'avl_nearest';
{
 * Add a single node to the tree.
 * The node must not be in the tree, and it must not
 * compare equal to any other node already in the tree.
 *
 * node   - the node to add
  }
procedure avl_add(tree:Pavl_tree_t; node:pointer);cdecl;external external_avl_library name 'avl_add';
{
 * Remove a single node from the tree.  The node must be in the tree.
 *
 * node   - the node to remove
  }
procedure avl_remove(tree:Pavl_tree_t; node:pointer);cdecl;external external_avl_library name 'avl_remove';
{
 * Reinsert a node only if its order has changed relative to its nearest
 * neighbors. To optimize performance avl_update_lt() checks only the previous
 * node and avl_update_gt() checks only the next node. Use avl_update_lt() and
 * avl_update_gt() only if you know the direction in which the order of the
 * node may change.
  }
function avl_update(_para1:Pavl_tree_t; _para2:pointer):boolean_t;cdecl;external external_avl_library name 'avl_update';
function avl_update_lt(_para1:Pavl_tree_t; _para2:pointer):boolean_t;cdecl;external external_avl_library name 'avl_update_lt';
function avl_update_gt(_para1:Pavl_tree_t; _para2:pointer):boolean_t;cdecl;external external_avl_library name 'avl_update_gt';
{
 * Return the number of nodes in the tree
  }
function avl_numnodes(tree:Pavl_tree_t):ulong_t;cdecl;external external_avl_library name 'avl_numnodes';
{
 * Return B_TRUE if there are zero nodes in the tree, B_FALSE otherwise.
  }
function avl_is_empty(tree:Pavl_tree_t):boolean_t;cdecl;external external_avl_library name 'avl_is_empty';
{
 * Used to destroy any remaining nodes in a tree. The cookie argument should
 * be initialized to NULL before the first call. Returns a node that has been
 * removed from the tree and may be free()'d. Returns NULL when the tree is
 * empty.
 *
 * Once you call avl_destroy_nodes(), you can only continuing calling it and
 * finally avl_destroy(). No other AVL routines will be valid.
 *
 * cookie - a "void *" used to save state between calls to avl_destroy_nodes()
 *
 * EXAMPLE:
 *	avl_tree_t *tree;
 *	struct my_data *node;
 *	void *cookie;
 *
 *	cookie = NULL;
 *	while ((node = avl_destroy_nodes(tree, &cookie)) != NULL)
 *		free(node);
 *	avl_destroy(tree);
  }
function avl_destroy_nodes(tree:Pavl_tree_t; cookie:Ppointer):pointer;cdecl;external external_avl_library name 'avl_destroy_nodes';
{
 * Final destroy of an AVL tree. Arguments are:
 *
 * tree   - the empty tree to destroy
  }
procedure avl_destroy(tree:Pavl_tree_t);cdecl;external external_avl_library name 'avl_destroy';


implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function AVL_NEXT(tree,node : longint) : longint;
begin
  abort;
 //  AVL_NEXT:=avl_walk(tree,node,AVL_AFTER);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function AVL_PREV(tree,node : longint) : longint;
begin
  abort;
  //AVL_PREV:=avl_walk(tree,node,AVL_BEFORE);
end;

end.

