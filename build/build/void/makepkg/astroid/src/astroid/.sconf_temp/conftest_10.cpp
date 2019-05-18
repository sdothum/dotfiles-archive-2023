
# include <notmuch.h>

int main (int argc, char ** argv)
{
  notmuch_database_t * nm_db;
  const char * uuid;
  notmuch_database_get_revision (nm_db, &uuid);

  return 0;
}
