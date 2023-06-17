import { Router, Request, Response } from 'express';

const router: Router = Router();

// Define a route for user registration
router.post('/register', (req: Request, res: Response) => {

  // Handle user registration logic
});

router.get('/test', (req: Request, res: Response) => {
    
    res.send('working')
  });
  

// Define a route for user login
router.post('/login', (req: Request, res: Response) => {
  // Handle user login logic
});

export default router;