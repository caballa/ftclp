#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "critbit.h"

typedef struct {
  char ** dict;
  int size;
} Dict;

Dict create_dict(){ 
  Dict d;
  d.dict = NULL;
  d.size = 0;
  return d; 
}  

void add_dict_string(Dict * dictionary, const char * string){

  char * copy = (char *)  malloc(sizeof(char) * (strlen(string) + 1));
  strcpy(copy,string);

  dictionary->dict = realloc(dictionary->dict, dictionary->size +1);
  dictionary->dict[dictionary->size] = copy;
  dictionary->size++;
  
  return;
}
void del_dict(Dict dictionary){
  int n = dictionary.size;
  int i;
  for (i=0;i<n;i++){
    free(dictionary.dict[i]);
  }
}
void print_dict(Dict dictionary){
  if (dictionary.size == 0){
    printf("EMPTY\n");
    return;
  }
  unsigned i = 0;
  printf("{");
  while (i < dictionary.size){
    printf("%s ;", dictionary.dict[i]);
    i++;
  }
  printf("}\n");

  return;
}

static const char *dict[] = {
  "p/1-q/1-r/1", "p/1-q/1-r/2","p/1-q/2-r/1","p/1-q/2-r/2", NULL
};

static int count_cb(const char *s, void *n) { (*(int *)n)++; return 0; }
static int insert_answer_cb(const char *s, void *n) {
  Dict * answers = (Dict *) n;
  add_dict_string(answers, s);
  return 0;
}

void print_answers(cb_tree_t *tree, const char * p){

  Dict answers = create_dict();

  /* if (!cb_tree_contains(tree, p)) { */
  /*   printf("No answers!\n");     */
  /*   return; */
  /* } */
  
  if (cb_tree_walk_prefixed(tree, p, insert_answer_cb, &answers) != 0) {
    fprintf(stderr, "Walking with empty prefix failed\n");
    return;
  }
  int size = 0;
  if (cb_tree_walk_prefixed(tree, p, count_cb, &size) != 0) {
      fprintf(stderr, "Walking with empty prefix failed\n");
      return;
    }
  printf("Number of answers: %d\n",size);
  printf("Answers=");
  print_dict(answers);

   /* del_dict(answers); */
  return;
}

int main(int argc, char **argv)
{
  unsigned i;
  cb_tree_t tree = cb_tree_make();


  for (i = 0; dict[i]; ++i){
    if (cb_tree_insert(&tree, dict[i]) != 0) {
      fprintf(stderr, "Insertion failed\n");
      return 0;
    }
    printf("Inserted %s\n",dict[i]);
  }

  const char *p1 = "p/1-q/1"; 
  const char *p2 = "p/1-q/2"; 
  const char *p3 = "p/1"; 
  const char *p4 = "s";
  const char *p5 = "p/1-q/1-r/1"; 
  const char *p6 = "p/1-q/1-r/2"; 
  const char *p7 = "p/1-q/2-r/1"; 
  const char *p8 = "p/1-q/2-r/2"; 
  printf("Printing answers for %s ... \n",p1);
  print_answers(&tree, p1);
  printf("Printing answers for %s ... \n",p2); 
  print_answers(&tree, p2);
  printf("Printing answers for %s ... \n",p3);
  print_answers(&tree, p3);
  printf("Printing answers for %s ... \n",p4);
  print_answers(&tree, p4);
  printf("Printing answers for %s ... \n",p5);
  print_answers(&tree, p5);
  printf("Printing answers for %s ... \n",p6);
  print_answers(&tree, p6);
  printf("Printing answers for %s ... \n",p7);
  print_answers(&tree, p7);
  printf("Printing answers for %s ... \n",p8);
  print_answers(&tree, p8);

  cb_tree_clear(&tree);
  return 1;
  
}
