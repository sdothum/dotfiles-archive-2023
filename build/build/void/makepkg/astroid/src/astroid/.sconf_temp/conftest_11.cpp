
# include <notmuch.h>

int main (int argc, char ** argv)
{
  notmuch_query_t * q;
  notmuch_threads_t * t;
  notmuch_status_t st;

  st = notmuch_query_search_threads_st (q, &t);

  return 0;
}
