NAME    := ComputorV2.jar

all:
	mvn package
	mv ./target/ComputorV-2-jar-with-dependencies.jar ./$(NAME)
	chmod u+x $(NAME)

clean:
	mvn clean

fclean: clean
	rm -rf $(NAME)

re: fclean
	$(MAKE) all

.PHONY: clean fclean re all

.SILENT: clean fclean re all