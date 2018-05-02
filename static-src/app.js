const config = {
    type: Phaser.AUTO,
    width: 800,
    height: 600,
    physics: {
        default: 'arcade',
        arcade: {
            gravity: { y: 200 }
        }
    },
    scene: {
        preload: preload,
        create: create
    }
};

const game = new Phaser.Game(config);

function preload () {
    //        this.load.setBaseURL('http://labs.phaser.io');
    this.load.setBaseURL('/static/');
    this.load.image('man', 'assets/thisman02-vx.png');
    this.load.image('sky', 'assets/space3.png');
    this.load.image('red', 'assets/red.png');
    //         this.load.image('logo', 'assets/sprites/phaser3-logo.png');
}

function create () {
    // this.add.image(400, 300, 'sky');

    var particles = this.add.particles('red');
    var emitter = particles.createEmitter({
        speed: 200,
        scale: { start: 2, end: 1 },
        blendMode: 'ADD'
    });

    var logo = this.physics.add.image(400, 100, 'man');

    logo.setVelocity(100, 200);
    logo.setBounce(1, 1);
    logo.setCollideWorldBounds(true);

    emitter.startFollow(logo);
}
