/*
 * Dump the ZFS pool configuration nvlist. In theory 'zdb -C' would do
 * this; in practice, it doesn't dump *all* of the pool config because
 * it dumps the in-kernel version and that appears to omit the spares.
 * This dumps the version obtained by by zpool_config().
 *
 * We reuse the actual dumping code from zdb, because why rewrite
 * nvlist dumping code when we don't have to? We just feed it from
 * a different data source.
 *
 * (That presumably makes this covered by the CDDL copyright by
 * the usual contamination mechanism.)
 *
 * Copyright: CDDL, presumed.
 *
 * Compile with:
 *	gcc -O2 -o zpconfig zpconfig.c -lzfs -lnvpair
 */
#include	<unistd.h>
#include	<stdio.h>
#include	<libzfs.h>

/* force assert() to always be an assert */
#undef	NDEBUG
#include	<assert.h>

/* VERIFY() requires that its argument always be evaluated. */
#define	VERIFY(X)	(assert(X))

char	*progname = "zpconfig";
int	status = 0;
libzfs_handle_t	*zph;

/* This is incomplete, which is unfortunate because there seems to be
   fascinating information in the vdev_stat_t structure. TODO: write
   a full dumper (and figure out how not to overwhelm us with the flood
   of information).
   See sys/fs/zfs.h in general. */
static void
dump_zfs_state(nvpair_t *elem, int indent)
{
	vdev_stat_t *vs;
	uint_t	c;

	VERIFY(nvpair_value_uint64_array(elem, (uint64_t **) &vs, &c) == 0);
	printf("%*sstate=%llu\n", indent, "", (u_longlong_t) vs->vs_state);
	printf("%*saux=%llu\n", indent, "", (u_longlong_t) vs->vs_aux);
	printf("%*s...\n", indent, "");
}
	
	
/* straight out of zdb.c, well, mostly: */
/*FOS: removed static */
/*FOS: orig: static void dump_nvlist(nvlist_t *list, int indent) */

void
dump_nvlist(nvlist_t *list, int indent)
{
	nvpair_t *elem = NULL;

	while ((elem = nvlist_next_nvpair(list, elem)) != NULL) {
		switch (nvpair_type(elem)) {
		case DATA_TYPE_STRING:
			{
				char *value;

				VERIFY(nvpair_value_string(elem, &value) == 0);
				(void) printf("%*s%s='%s'\n", indent, "",
				    nvpair_name(elem), value);
			}
			break;

		case DATA_TYPE_UINT64:
			{
				uint64_t value;

				VERIFY(nvpair_value_uint64(elem, &value) == 0);
				(void) printf("%*s%s=%llu\n", indent, "",
				    nvpair_name(elem), (u_longlong_t)value);
			}
			break;

		case DATA_TYPE_NVLIST:
			{
				nvlist_t *value;

				VERIFY(nvpair_value_nvlist(elem, &value) == 0);
				(void) printf("%*s%s\n", indent, "",
				    nvpair_name(elem));
				dump_nvlist(value, indent + 4);
			}
			break;

		case DATA_TYPE_NVLIST_ARRAY:
			{
				nvlist_t **value;
				uint_t c, count;

				VERIFY(nvpair_value_nvlist_array(elem, &value,
				    &count) == 0);

				for (c = 0; c < count; c++) {
					(void) printf("%*s%s[%u]\n", indent, "",
					    nvpair_name(elem), c);
					dump_nvlist(value[c], indent + 8);
				}
			}
			break;

			/* ZFS stores disk state as a vdev_state_t structure
			   that's claimed to be a UINT64 array. */
		    case DATA_TYPE_UINT64_ARRAY:
			if (strcmp(nvpair_name(elem), "vdev_stats") == 0) {
				printf("%*sstats:\n", indent, "");
				dump_zfs_state(elem, indent+4);
			} else
				(void) printf("%*s%s=<array of uint64_t>\n",
					      indent, "", nvpair_name(elem));
			break;

		default:

		    /*(void) printf("bad config type %d for %s\n",
		      nvpair_type(elem), nvpair_name(elem)); */
		    (void) printf("%*sUNHANDLED: %s=<NVPAIR TYPE %d>\n",
				  indent, "", nvpair_name(elem),
				  nvpair_type(elem));
		}
	}
}
/* end straight out of etc */

void
dump_config(char *pooln)
{
	zpool_handle_t	*zp = NULL;
	nvlist_t	*zs;

	zp = zpool_open(zph, pooln);
	if (!zp) {
		fprintf(stderr, "%s: cannot open pool %s\n", progname, pooln);
		status = 1;
		goto done;
	}
	zs = zpool_get_config(zp, NULL);
	printf("%s\n", pooln);
	dump_nvlist(zs, 4);

done:
	if (zp) zpool_close(zp);
	return;
}

int
main(int argc, char **argv)
{
	int i;
	zph = libzfs_init();
	for (i = 1; i < argc; i ++)
		dump_config(argv[i]);
	libzfs_fini(zph);
	return status;
}
