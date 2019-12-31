
# include <notmuch.h>

int main (int argc, char ** argv)
{
  notmuch_query_t * q;
  unsigned int c;
  notmuch_status_t st;

  st = notmuch_query_count_threads_st (q, &c);

  return 0;
}
