public class Node {

    private int value;

    private Node leftChild;
    private Node rightChild;

    public Node (int value){
        this.value = value;
    }

    public void insert(Node n){
        if (value > n.value) setLeftChild(n);
        if (value < n.value) setRightChild(n);
    }

    public void setLeftChild(Node leftChild) {
        this.leftChild = leftChild;
    }

    public void setRightChild(Node rightChild) {
        this.rightChild = rightChild;
    }

    public Object getValue() {
        return value;
    }

    public Node getLeftChild() {
        return leftChild;
    }

    public Node getRightChild() {
        return rightChild;
    }
}
